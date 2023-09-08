pacman::p_load(tidyverse)
dados <- iris
coluna <- c(1:150)
dados <- cbind(dados, coluna)

dados %>% distinct(Species)
dados <- dados %>% 
  mutate(
    Species = recode(Species,
                     setosa = "Setosa",
                     versicolor = "Versicolor",
                     virginica = "Virginica")
  )
dados %>% head()

# grafico de linhas para as medias de sepal.length para cada especie
dados %>%
  group_by(Species) %>% 
  summarise(media = round(mean(Sepal.Length), 2),
            variancia = var(Sepal.Length)) %>% 
  ggplot2::ggplot(aes(Species, media, group=1)) +
  geom_line(stat = "identity", color = "black", size = 0.5) +
  geom_point(aes(color = Species), size = 2.7) +
  geom_text(aes(label = media), nudge_x = -0.1,nudge_y = 0.1) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  labs(title = "Média sepal.length por espécie",
       x ="",
       y = "Média",
       color="Especies")

# grafico de linhas
dados %>% 
  filter(Species %in% c("Setosa", "Virginica")) %>% 
  ggplot(aes(coluna, Sepal.Length)) +
  geom_line()


# grafico de boxplot de sepal.length para cada especie 
dados %>% 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot(outlier.color = "red") +
  labs(fill = "Espécies",
       title = "Distribuições do comprimento de sépalas",
       x = "") +
  scale_fill_manual(values = c("#7C86F1", "#0EF4C0", "#A83BE7")) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


# grafico de correlacao para cada especie
dados %>% 
  ggplot(aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 3) +
  facet_grid(~Species) +
  scale_color_manual(values = c("#7C86F1", "#0EF4C0", "#A83BE7")) +
  labs(title = "Correlação entre comprimento e largura",
       color = "Espécies",
       x = "Largura",
       y = "Comprimento") +
  theme(plot.title = element_text(face = "bold"))

# rio::export(x = dados, file = "iris.xlsx")



