# Credit Risk Modeling: Interpretability vs. Performance (IFRS9) ⚖️

Este projeto apresenta um benchmark estratégico para modelagem de Probabilidade de Default (PD), comparando o padrão-ouro regulatório (**Regressão Logística + WoE**) com modelos de alta performance (**XGBoost**).

O objetivo central é discutir o trade-off entre a precisão preditiva e a explicabilidade exigida por órgãos reguladores e normas como a **IFRS9**.

## 🚀 Destaques do Estudo
- **Abordagem Estratégica:** Foco em transparência para auditoria.
- **Tech Stack:** R (Ecosystem `tidymodels`, `embed`, `xgboost`, `patchwork`).
- **Feature Engineering:** Implementação de Weight of Evidence (WoE) e discretização de variáveis.
- **Métricas:** Comparação de Gini e AUC-ROC.

## 📊 Principais Descobertas
Embora o **XGBoost** tenha apresentado uma performance superior (Gini: 0.209), a **Regressão Logística** (Gini: 0.205) mostrou-se mais vantajosa para o contexto de negócio devido à sua natureza linear e facilidade de explicação por meio dos pesos de evidência (WoE).

### Visualização do Risco
> Aqui, o modelo transforma variáveis contínuas em uma "escada de risco" intuitiva.



## 🛠️ Como Reproduzir
1. Clone este repositório:
   ```bash
   git clone [https://github.com/teu-usuario/p1_risk.git](https://github.com/teu-usuario/p1_risk.git)
