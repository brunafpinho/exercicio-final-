EXERCÍCIO NO R - 1
# install.packages("dplyr")

# Função para calcular o VPL
calcular_vpl <- function(fluxo_caixa, taxa_desconto, n = 1) {
  if (length(fluxo_caixa) == 0) {
    return(0)
  } else {
    # VPL do primeiro fluxo de caixa descontado
    vpl_atual <- fluxo_caixa[1] / ((1 + taxa_desconto) ^ n)
    # Chamada recursiva para os fluxos de caixa restantes
    return(vpl_atual + calcular_vpl(fluxo_caixa[-1], taxa_desconto, n + 1))
  }
}

# Passo 1: Gerar o fluxo de caixa simulado
set.seed(123) # Para reprodutibilidade
fluxo_caixa <- rnorm(100, mean = 5000, sd = 1000)

# Passo 2: Calcular o VPL para taxas de desconto de 0.01 a 0.10
taxas_desconto <- seq(0.01, 0.10, by = 0.01)
resultados_vpl <- data.frame(Taxa_Desconto = taxas_desconto, VPL = NA)

for (i in 1:length(taxas_desconto)) {
  taxa <- taxas_desconto[i]
  resultados_vpl$VPL[i] <- calcular_vpl(fluxo_caixa, taxa)
}

# Passo 3: Exibir os resultados
print(resultados_vpl)

# Plotando os resultados
plot(resultados_vpl$Taxa_Desconto, resultados_vpl$VPL, type = "b",
     main = "VPL para diferentes taxas de desconto",
     xlab = "Taxa de Desconto", ylab = "VPL",
     col = "blue", pch = 16)

_________________________________________

EXERCÍCIO NO PYTHON – 2
# Classe base: Investimento
class Investimento:
    def _init_(self, nome, quantidade, valor_unitario):
        self.nome = nome  # Nome do investimento
        self.quantidade = quantidade  # Quantidade possuída
        self.valor_unitario = valor_unitario  # Valor por unidade

    def valor_total(self):
        """Calcula o valor total do investimento."""
        return self.quantidade * self.valor_unitario

# Subclasse para Ações
class Acao(Investimento):
    def _init_(self, nome, quantidade, valor_unitario, dividendo=0):
        super()._init_(nome, quantidade, valor_unitario)
        self.dividendo = dividendo  # Dividendos pagos pela ação

    def valor_total_com_dividendo(self):
        """Calcula o valor total com dividendos."""
        return self.valor_total() + (self.dividendo * self.quantidade)

# Subclasse para Títulos
class Titulo(Investimento):
    def _init_(self, nome, quantidade, valor_unitario, taxa_juros):
        super()._init_(nome, quantidade, valor_unitario)
        self.taxa_juros = taxa_juros  # Taxa de juros do título

    def rendimento_anual(self):
        """Calcula o rendimento anual do título."""
        return self.valor_total() * self.taxa_juros

# Subclasse para Fundos Mútuos
class FundoMutuo(Investimento):
    def _init_(self, nome, quantidade, valor_unitario, taxa_administracao):
        super()._init_(nome, quantidade, valor_unitario)
        self.taxa_administracao = taxa_administracao  # Taxa de administração

    def valor_com_taxa(self):
        """Calcula o valor do fundo após aplicar a taxa de administração."""
        return self.valor_total() - (self.valor_total() * self.taxa_administracao)

# Classe Portfolio para gerenciar os investimentos
class Portfolio:
    def _init_(self):
        self.investimentos = []

    def adicionar_investimento(self, investimento):
        """Adiciona um investimento ao portfólio."""
        self.investimentos.append(investimento)

    def remover_investimento(self, investimento):
        """Remove um investimento do portfólio."""
        self.investimentos.remove(investimento)

    def valor_total_portfolio(self):
        """Calcula o valor total do portfólio."""
        return sum(invest.valor_total() for invest in self.investimentos)

    def relatorio(self):
        """Gera um relatório do portfólio."""
        print("\nRelatório do Portfólio:")
        for invest in self.investimentos:
            print(f"Investimento: {invest.nome}, Valor Total: R$ {invest.valor_total():.2f}")
        print(f"\nValor Total do Portfólio: R$ {self.valor_total_portfolio():.2f}")

# Testando as classes
if _name_ == "_main_":
    # Criando o portfólio
    meu_portfolio = Portfolio()

    # Adicionando investimentos
    meu_portfolio.adicionar_investimento(Acao("AAPL", 10, 150, dividendo=2))
    meu_portfolio.adicionar_investimento(Titulo("US Treasury", 5, 1000, taxa_juros=0.05))
    meu_portfolio.adicionar_investimento(FundoMutuo("Fundo XYZ", 50, 200, taxa_administracao=0.02))

    # Gerando relatório do portfólio
    meu_portfolio.relatorio()

    # Exemplos de cálculos específicos
    acao = Acao("AAPL", 10, 150, dividendo=2)
    print(f"Valor total da ação com dividendos: R$ {acao.valor_total_com_dividendo():.2f}")

    titulo = Titulo("US Treasury", 5, 1000, taxa_juros=0.05)
    print(f"Rendimento anual do título: R$ {titulo.rendimento_anual():.2f}")

#resultado
Relatório do Portfólio:
Investimento: AAPL, Valor Total: R$ 1500.00
Investimento: US Treasury, Valor Total: R$ 5000.00
Investimento: Fundo XYZ, Valor Total: R$ 10000.00

Valor Total do Portfólio: R$ 16500.00
Valor total da ação com dividendos: R$ 1520.00
Rendimento anual do título: R$ 250.00
