import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import pyreadr
result = pyreadr.read_r("/Users/fabiogiovanardi/Desktop/dati_semestrali_2019_2023_v1.RData")
df = list(result.values())[0]

# 1. Esplora variabili numeriche
variabili_numeriche = ['Eta', 'GG', 'GradoMenomazione']
for col in variabili_numeriche:
    print(f"\n==== {col} ====")
    print(df[col].describe())
    sns.histplot(df[col], bins=50, kde=True)
    plt.title(f"Distribuzione di {col}")
    plt.grid()
    plt.show()

# 2. Verifica outlier con boxplot
for col in variabili_numeriche:
    plt.figure()
    sns.boxplot(x=df[col])
    plt.title(f"Boxplot di {col}")
    plt.grid()
    plt.show()

# 3. Esplora variabili categoriche
variabili_categoriche = ['Genere', 'ATECO_1', 'REGIONE_1']
for col in variabili_categoriche:
    print(f"\n==== {col} ====")
    print(df[col].value_counts())
    print(f"Cardinalità di {col}: {df[col].nunique()}")

# 4. Rimozione eventuali categorie rare (es. ATECO_1 con frequenza < 1000)
soglia_minima = 1000
df = df[df['ATECO_1'].map(df['ATECO_1'].value_counts()) > soglia_minima]

# 5. Visualizza valori nulli
print("\n==== Valori nulli ====")
print(df.isnull().sum())