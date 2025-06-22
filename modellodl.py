import pyreadr
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import optuna
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, precision_score, recall_score, f1_score, confusion_matrix
from imblearn.over_sampling import SMOTE
from torch.utils.data import DataLoader, TensorDataset
import seaborn as sns
import matplotlib.pyplot as plt
import joblib

# === 1. CARICAMENTO DATI RDATA ===
result = pyreadr.read_r("/Users/fabiogiovanardi/Desktop/dati_semestrali_2019_2023_v1.RData")
df = list(result.values())[0]

# === 2. PREPROCESSING ===
df = df[['Eta', 'Genere', 'GG', 'GradoMenomazione', 'ATECO_1', 'REGIONE_1']].dropna()
df = df[(df['Eta'] > 0) & (df['Eta'] <= 100)]
df = df[df['GG'] >= 0]

# Target binario: InfortunioGrave = 1 se GG > 40
df['InfortunioGrave'] = (df['GG'] > 40).astype(int)

# Dummies e scalatura
df_encoded = pd.get_dummies(df.drop(columns='InfortunioGrave'))
scaler = StandardScaler()
X_scaled = scaler.fit_transform(df_encoded)
y = df['InfortunioGrave'].values

# SMOTE per bilanciamento
X_res, y_res = SMOTE().fit_resample(X_scaled, y)

# Tensori
X_tensor = torch.tensor(X_res, dtype=torch.float32)
y_tensor = torch.tensor(y_res, dtype=torch.int)

# Train-test split
X_train, X_val, y_train, y_val = train_test_split(X_tensor, y_tensor, test_size=0.2, random_state=42)
train_loader = DataLoader(TensorDataset(X_train), batch_size=64, shuffle=True)

# === 3. CLASSE MemAE ===
class MemAE(nn.Module):
    def __init__(self, input_dim, memory_size=50, shrink_thres=0.0025, dropout_rate=0.2):
        super(MemAE, self).__init__()
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, 128), nn.LeakyReLU(),
            nn.Dropout(dropout_rate),
            nn.Linear(128, 64), nn.LeakyReLU(),
            nn.Dropout(dropout_rate),
            nn.Linear(64, 32)
        )
        self.memory = nn.Parameter(torch.randn(memory_size, 32))
        self.shrink_thres = shrink_thres
        self.decoder = nn.Sequential(
            nn.Linear(32, 64), nn.LeakyReLU(),
            nn.Dropout(dropout_rate),
            nn.Linear(64, 128), nn.LeakyReLU(),
            nn.Dropout(dropout_rate),
            nn.Linear(128, input_dim), nn.Sigmoid()
        )

    def forward(self, x):
        z = self.encoder(x)
        att_score = torch.matmul(z, self.memory.T)
        att_weight = F.softmax(att_score, dim=1)
        if self.shrink_thres > 0:
            att_weight = F.softshrink(att_weight, lambd=self.shrink_thres)
        mem_comb = torch.matmul(att_weight, self.memory)
        return self.decoder(mem_comb)

# === 4. OPTUNA OBJECTIVE ===
def objective(trial):
    memory_size = trial.suggest_int('memory_size', 20, 40)
    shrink_thres = trial.suggest_float('shrink_thres', 0.001, 0.1)
    lr = trial.suggest_float('lr', 1e-4, 1e-2)
    dropout_rate = trial.suggest_float('dropout_rate', 0.0, 0.3)
    threshold_perc = trial.suggest_int('threshold', 90, 99)

    model = MemAE(input_dim=X_tensor.shape[1], memory_size=memory_size,
                  shrink_thres=shrink_thres, dropout_rate=dropout_rate)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    model.train()
    for epoch in range(10):
        for xb in train_loader:
            x_batch = xb[0]
            recon = model(x_batch)
            loss = F.mse_loss(recon, x_batch)
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

    model.eval()
    with torch.no_grad():
        recon = model(X_val)
        errors = torch.mean((X_val - recon) ** 2, dim=1).numpy()
        threshold = np.percentile(errors, threshold_perc)
        preds = (errors > threshold).astype(int)
        labels = y_val.numpy()

        auc = roc_auc_score(labels, errors)
        trial.set_user_attr("auc", auc)
        return auc

# === 5. ESECUZIONE TUNING (pochi trial per velocitÃ ) ===
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=3)

# Salva lo study per evitare perdita dati
joblib.dump(study, "optuna_study.pkl")

print("Best parameters:", study.best_params)
print("Best AUC:", study.best_value)

# === 6. ESECUZIONE FINALE MIGLIORE MODELLO ===
params = study.best_params
model = MemAE(input_dim=X_tensor.shape[1], memory_size=params['memory_size'],
              shrink_thres=params['shrink_thres'], dropout_rate=params['dropout_rate'])
optimizer = torch.optim.Adam(model.parameters(), lr=params['lr'])

for epoch in range(15):
    for xb in train_loader:
        recon = model(xb[0])
        loss = F.mse_loss(recon, xb[0])
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

# === 7. EVALUATION ===
model.eval()
with torch.no_grad():
    recon = model(X_val)
    errors = torch.mean((X_val - recon) ** 2, dim=1).numpy()
    threshold = np.percentile(errors, params['threshold'])
    preds = (errors > threshold).astype(int)
    labels = y_val.numpy()

auc = roc_auc_score(labels, errors)
precision = precision_score(labels, preds)
recall = recall_score(labels, preds)
f1 = f1_score(labels, preds)

print(f"AUC: {auc:.4f} | Precision: {precision:.4f} | Recall: {recall:.4f} | F1: {f1:.4f}")

# === 8. CONFUSION MATRIX ===
cm = confusion_matrix(labels, preds)
sns.heatmap(pd.DataFrame(cm, index=["Real 0", "Real 1"], columns=["Pred 0", "Pred 1"]),
            annot=True, fmt='d')
plt.title("Confusion Matrix")
plt.show()

# === 9. SALVATAGGIO MODELLO ===
torch.save(model.state_dict(), "best_memae_inail.pt")
print("Modello salvato in best_memae_inail.pt")