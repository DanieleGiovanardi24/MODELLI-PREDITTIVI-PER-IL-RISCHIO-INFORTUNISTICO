import pandas as pd
import numpy as np
import pyreadr
import torch
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from scipy.stats import ks_2samp
import geopandas as gpd

# === 1. CARICAMENTO DATI ===
df_anomalie = pd.read_csv("/Users/fabiogiovanardi/Desktop/cartella senza nome/RISULTATI/anomalie_rilevate.csv")

# === 2. STATISTICHE DESCRITTIVE ===
print("\nSTATISTICHE DESCRITTIVE")
print(df_anomalie[['Eta', 'GG', 'GradoMenomazione']].describe())

# === 3. DISTRIBUZIONE GG PER DECILI ===
print("\nDistribuzione GG (decili)")
gg_decili = pd.qcut(df_anomalie['GG'], q=10, duplicates='drop')
print(gg_decili.value_counts().sort_index())

# === 4. DISTRIBUZIONE ETÀ ===
eta_bins = pd.cut(df_anomalie['Eta'], bins=[0, 25, 35, 45, 55, 65, 100], right=False)
print("\nDistribuzione Età per fasce")
print(eta_bins.value_counts().sort_index())

# === 5. TEST KS PER GG vs Età ===
ks_stat, p_value = ks_2samp(df_anomalie['GG'], df_anomalie['Eta'])
print(f"\nKS test GG vs Eta: statistic={ks_stat:.4f}, p-value={p_value:.4f}")

# === 6. GRAFICI ===
# Top 10 ATECO
top_ateco = df_anomalie.filter(like="ATECO_1_").sum().sort_values(ascending=False).head(10)
top_ateco.plot(kind='bar', title="Top 10 settori ATECO tra le anomalie")
plt.tight_layout()
plt.savefig("top_ateco_anomalie.png")
plt.clf()

# Boxplot GG per Età
df_anomalie['Eta_bin'] = pd.cut(df_anomalie['Eta'], bins=[0, 25, 35, 45, 55, 65, 100], right=False)
sns.boxplot(data=df_anomalie, x='Eta_bin', y='GG')
plt.title("Boxplot GG per fascia di età")
plt.tight_layout()
plt.savefig("boxplot_eta_gg.png")
plt.clf()

# Clustering
X_clust = df_anomalie[['Eta', 'GG']]
kmeans = KMeans(n_clusters=4, random_state=0).fit(X_clust)
df_anomalie['cluster'] = kmeans.labels_

sns.scatterplot(data=df_anomalie, x='Eta', y='GG', hue='cluster', palette='Set2')
plt.title("KMeans clustering (Eta, GG)")
plt.tight_layout()
plt.savefig("kmeans_eta_gg.png")
plt.clf()

# === 7. MAPPATURA CLUSTER SU ITALIA ===
# Conta anomalie per regione
anomaly_counts = df_anomalie['REGIONE_1'].value_counts().rename_axis('REGIONE').reset_index(name='anomalie')

# Unisci con shapefile
gdf['REGIONE'] = gdf['REGIONE'].str.upper()
anomaly_counts['REGIONE'] = anomaly_counts['REGIONE'].str.upper()
merged = gdf.merge(anomaly_counts, on='REGIONE', how='left')

# Plot heatmap
fig, ax = plt.subplots(figsize=(12, 10))
merged.plot(column='anomalie', cmap='OrRd', ax=ax, legend=True, edgecolor='black')
ax.set_title("Numero di anomalie per regione", fontsize=16)
ax.axis('off')
plt.tight_layout()
plt.savefig("heatmap_anomalie_per_regione.png")
plt.show()