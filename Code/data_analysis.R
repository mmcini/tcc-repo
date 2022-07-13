########## DADOS NÃO INCLUÍDOS NO REPOSITÓRIO ####################################

library(RColorBrewer)
library(tidyverse)
library(prospectr)
library(ggpubr)
library(scales)

### PUBLICACOES ################################################################
# Carregando dados
publication_years <- read_csv("../Dados/Publicacoes/publications_per_year.csv")

# only publications after 1980 excluding 2022
publication_years <- publication_years[which(publication_years$year >= 1990 &
                                             publication_years$year < 2022), ]

publication_years  <-  publication_years %>%
                       pivot_longer(c(sensor:pxrf),
                                    names_to = "sensors",
                                    values_to = "publications")

publication_years$sensors <- factor(publication_years$sensors,
                             levels = c("sensor", "pxrf", "visnir", order = T))

publications_plot <- ggplot(publication_years,
                            aes(x = year, y = publications, fill = sensors)) +
                     geom_bar(stat = "identity", position = position_dodge()) +
                     ylab("Número de publicações sobre solos") + xlab("Ano")  +
                     scale_fill_discrete(name = "Palavras-chave",
                                    labels = c("Proximal sensing", "pXRF", "Vis-NIR")) +
                     theme_bw() +
                     theme(text = element_text(family = "Times New Roman"))

ggsave("publication_numbers.png", publications_plot, device = "png",
       width = 150, height = 80, units = "mm")

### PXRF ######################################################################
# Carregando dados
pxrf_ox_ufla <- read_csv("../Dados/Perfis/pxrf_ox_ufla.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Profundidade = Lat) %>%
    replace(is.na(.), 0)
pxrf_ox_quartz <- read_csv("../Dados/Perfis/pxrf_ox_quartz.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Profundidade = Lat) %>%
    replace(is.na(.), 0)
pxrf_glei_ufla <- read_csv("../Dados/Perfis/pxrf_glei_ufla.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Profundidade = Lat) %>%
    replace(is.na(.), 0)

column_names <- c("Profundidade", "Al", "Ca", "Mg",
                "Fe", "Si", "Ti", "Zr", "Ti/Zr")

# Plots
## Variaveis utilizadas para os plots
plot_vars <- column_names[-1]

## Base dos plots
pxrf_plots_ox_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_ox_ufla,
            aes_string(x = "Profundidade", y = variable))
    pxrf_plots_ox_ufla[[variable]] <- plot
}

pxrf_plots_ox_quartz <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_ox_quartz,
            aes_string(x = "Profundidade", y = variable))
    pxrf_plots_ox_quartz[[variable]] <- plot
}

pxrf_plots_glei_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_glei_ufla,
            aes_string(x = "Profundidade", y = variable))
    pxrf_plots_glei_ufla[[variable]] <- plot
}

## Lista com os plots-base
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
pxrf_plots <- list(ox_ufla = pxrf_plots_ox_ufla,
                   ox_quartz = pxrf_plots_ox_quartz,
                   glei_ufla = pxrf_plots_glei_ufla)

plot_titles <- list(ox_ufla = "Organossolo Háplico",
                    ox_quartz = "Cambissolo Háplico",
                    glei_ufla = "Gleissolo Háplico")

eixo_x <- rev(c(5, 20, 35, 50, 65, 80, 95, 110))
## Formatacoes
for (soil in soil_names) {
    count <- 1
    for (variable in plot_vars) {
        plot <- pxrf_plots[[soil]][[variable]] +
            geom_point() +
            geom_line() +
            coord_flip() +
            scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3)) +
            scale_x_continuous(breaks = seq(-105, 0, 15),
                               labels = eixo_x) +
            xlab("Profundidade (cm)") + ylab(plot_vars[count]) +
            theme_bw() +
            theme(text = element_text(family = "Times New Roman"),
                  axis.text = element_text(size = 7),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
        if (variable != "Al") {
            plot <- plot +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank())
        }
        if (variable == "Ca" | variable == "Fe" | variable == "Si") {
            plot <- plot +
                scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3),
                               labels = unit_format(unit = "K",
                                                       scale = 1e-3))
        }
        pxrf_plots[[soil]][[variable]] <- plot
        count <- count + 1
    }
}
column_names <- c("Profundidade", "Al", "Ca", "Mg",
                "Fe", "Si", "Ti", "Zr", "Ti/Zr")

## Arranjo de plots
pxrf_plot_arrange <- list()
for (soil in soil_names) {
    arrange <- ggarrange(pxrf_plots[[soil]][["Al"]],
                         pxrf_plots[[soil]][["Ca"]],
                         pxrf_plots[[soil]][["Mg"]],
                         pxrf_plots[[soil]][["Fe"]],
                         pxrf_plots[[soil]][["Si"]],
                         pxrf_plots[[soil]][["Ti"]],
                         pxrf_plots[[soil]][["Zr"]],
                         pxrf_plots[[soil]][["Ti/Zr"]],
                         nrow = 1, widths = c(1.3, 1, 1, 1, 1, 1, 1, 1))
    arrange <- annotate_figure(arrange, top = text_grob(plot_titles[soil],
                                             size = 12,
                                             family = "Times New Roman"))
    pxrf_plot_arrange[[soil]] <- arrange
}

pxrf_depth_plots <- ggarrange(pxrf_plot_arrange[["ox_ufla"]],
                         pxrf_plot_arrange[["ox_quartz"]],
                         pxrf_plot_arrange[["glei_ufla"]], nrow = 3)

ggsave("pxrf_depth.png", pxrf_depth_plots, device = "png",
       width = 250, height = 150, units = "mm", bg = "white")

### FERTILIDADE ###############################################################
# Carregando dados
fertility_ox_ufla <- read_csv("../Dados/Fertilidade/fertilidade_ox_ufla.csv")
fertility_ox_quartz <- read_csv("../Dados/Fertilidade/fertilidade_ox_quartz.csv")
fertility_glei_ufla <- read_csv("../Dados/Fertilidade/fertilidade_glei_ufla.csv")

# Arrumando nomes das colunas
column_names_symbols <- c("ID", "Protocolo", "Amostra", "Profundidade", "pH",
                          "K⁺", "P", "Na⁺", "Ca²⁺", "Mg²⁺", "Al³⁺", "H⁺+Al³⁺",
                          "SB", "t", "T", "V", "m", "M.O.", "P.Rem")

## Versao sem simbolos, pois algumas funcoes nao funcionam com simbolos
column_names <- c("ID", "Protocolo", "Amostra", "Profundidade", "pH", "K",
                  "P", "Na", "Ca", "Mg", "Al", "H+Al", "SB", "t",
                  "T", "V", "m", "M.O.", "P.Rem")

names(fertility_ox_ufla) <- column_names
names(fertility_ox_quartz) <- column_names
names(fertility_glei_ufla) <- column_names

# Plots
## Variaveis utilizadas para os plots
plot_vars <- column_names[c(6, 9:11, 16:18)]
y_labels <- column_names_symbols[c(6, 9:11, 16:18)]

## Base dos plots
fertility_plots_ox_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_ox_ufla,
            aes_string(x = "Profundidade", y = variable))
    fertility_plots_ox_ufla[[variable]] <- plot
}

fertility_plots_ox_quartz <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_ox_quartz,
            aes_string(x = "Profundidade", y = variable))
    fertility_plots_ox_quartz[[variable]] <- plot
}

fertility_plots_glei_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_glei_ufla,
            aes_string(x = "Profundidade", y = variable))
    fertility_plots_glei_ufla[[variable]] <- plot
}

## Lista com os plots-base
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
fertility_plots <- list(ox_ufla = fertility_plots_ox_ufla,
                        ox_quartz = fertility_plots_ox_quartz,
                        glei_ufla = fertility_plots_glei_ufla)

plot_titles <- list(ox_ufla = "Organossolo Háplico",
                    ox_quartz = "Cambissolo Háplico",
                    glei_ufla = "Gleissolo Háplico")

## Formatacoes
for (soil in soil_names) {
    count <- 1
    for (variable in plot_vars) {
        plot <- fertility_plots[[soil]][[variable]] +
            geom_point() +
            geom_line() +
            coord_flip() +
            scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3)) +
            scale_x_continuous(breaks = seq(-105, 0, 15),
                               labels = eixo_x) +
            xlab("Profundidade (cm)") + ylab(y_labels[count]) +
            theme_bw() +
            theme(text = element_text(family = "Times New Roman"),
                  axis.text = element_text(size = 7),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
        if (variable != "K") {
            plot <- plot +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank())
        }
        fertility_plots[[soil]][[variable]] <- plot
        count <- count + 1
    }
}

## Arranjo de plots
fertility_plot_arrange <- list()
for (soil in soil_names) {
    arrange <- ggarrange(fertility_plots[[soil]][["K"]],
                         fertility_plots[[soil]][["Ca"]],
                         fertility_plots[[soil]][["Mg"]],
                         fertility_plots[[soil]][["Al"]],
                         fertility_plots[[soil]][["V"]],
                         fertility_plots[[soil]][["m"]],
                         fertility_plots[[soil]][["M.O."]],
                         nrow = 1, widths = c(1.3, 1, 1, 1, 1, 1, 1))
    arrange <- annotate_figure(arrange, top = text_grob(plot_titles[soil],
                                             size = 12,
                                             family = "Times New Roman"))
    fertility_plot_arrange[[soil]] <- arrange
}

fertility_depth_plots <- ggarrange(fertility_plot_arrange[["ox_ufla"]],
                         fertility_plot_arrange[["ox_quartz"]],
                         fertility_plot_arrange[["glei_ufla"]], nrow = 3)

ggsave("fertility_depth.png", fertility_depth_plots, device = "png",
       width = 150, height = 130, units = "mm", bg = "white")

### TEXTURA ####################################################################
# Carregando dados
texture_ox_ufla <- read_csv("../Dados/Textura/textura_ox_ufla.csv")
texture_ox_quartz <- read_csv("../Dados/Textura/textura_ox_quartz.csv")
texture_glei_ufla <- read_csv("../Dados/Textura/textura_glei_ufla.csv")

column_names <- c("ID", "Protocolo", "Amostra", "Profundidade", "Argila",
                  "Silte", "Areia grossa", "Areia fina")

names(texture_ox_ufla) <- column_names
names(texture_ox_quartz) <- column_names
names(texture_glei_ufla) <- column_names

# Plots
## Variaveis utilizadas
plot_vars <- column_names[5:8]

## Lista com os dados
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
texture_data <- list(ox_ufla = texture_ox_ufla,
                        ox_quartz = texture_ox_quartz,
                        glei_ufla = texture_glei_ufla)

## Organizando os dados para os plots
for (soil in soil_names) {
    texture_data[[soil]] <- texture_data[[soil]] %>%
        pivot_longer(c(5:8), names_to = "Fração", values_to = "Teor (%)") %>%
        mutate(Fração = factor(Fração,
                                 levels = c("Areia grossa", "Areia fina",
                                            "Silte", "Argila"),
                                 ordered = T))
}

plot_titles <- list(ox_ufla = "Organossolo Háplico",
                    ox_quartz = "Cambissolo Háplico",
                    glei_ufla = "Gleissolo Háplico")

colors <- c("#B83100", "#008FF5", "#DBDB95", "#9E7757")

## Criacao dos plots
texture_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(texture_data[[soil]],
                   aes(x = Profundidade, y = `Teor (%)`, fill = Fração)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = `Teor (%)`),
                  position = position_fill(vjust = 0.5),
                  family = "Times New Roman",
                  size = 4) +
        coord_flip() + ggtitle(plot_titles[soil]) +
        scale_fill_manual(values = rev(colors)) +
        scale_y_continuous(position = "right",
                           breaks = seq(0, 1, 0.25),
                           labels = seq(0, 100, 25)) +
        scale_x_continuous(breaks = seq(-105, 0, 15),
                           labels = eixo_x,
                           limits = c(-112, 7)) +
        theme_bw() + xlab("Profundidade (cm)") +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
    texture_plots[[soil]] <- plot
}

texture_plots_arrange <- ggarrange(texture_plots[["ox_ufla"]],
                            texture_plots[["ox_quartz"]],
                            texture_plots[["glei_ufla"]],
                            nrow = 1, common.legend = T, legend = "bottom")


ggsave("texture_plots.png", texture_plots_arrange, device = "png",
       width = 210, height = 130, units = "mm", bg = "white")

### VIS-NIR ####################################################################

# Carrega os dados e calcula a media das repeticoes
# adiciona as profundidades
depth_order <- c("5 cm", "20 cm", "35 cm",
                 "50 cm", "65 cm", "80 cm",
                 "95 cm", "110 cm")

depth <- factor(c(rep("5 cm", 5), rep("20 cm", 5),
                  rep("35 cm", 5), rep("50 cm", 5),
                  rep("65 cm", 5), rep("80 cm", 5),
                  rep("95 cm", 5), rep("110 cm", 5)),
                  levels = depth_order, ordered = T)

visnir_ox_ufla <- read_csv("../Dados/Espectral/visnir_ox_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

visnir_ox_quartz <- read_csv("../Dados/Espectral/visnir_ox_quartz.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

visnir_glei_ufla <- read_csv("../Dados/Espectral/visnir_glei_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

# Dados sem continuum removal
## Organizando os dados para cria os plots
## agrupa dados por profundidade
visnir_ox_ufla <- visnir_ox_ufla %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

visnir_ox_quartz <- visnir_ox_quartz %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

visnir_glei_ufla <- visnir_glei_ufla %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
visnir_data <- list(ox_ufla = visnir_ox_ufla,
                        ox_quartz = visnir_ox_quartz,
                        glei_ufla = visnir_glei_ufla)

plot_titles <- list(ox_ufla = "Organossolo Háplico",
                    ox_quartz = "Cambissolo Háplico",
                    glei_ufla = "Gleissolo Háplico")

colors <- brewer.pal(8, "RdGy")

visnir_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(visnir_data[[soil]],
                   aes(x = `Banda (nm)`, y = `Refletância`)) +
        geom_line(aes(color = Profundidade)) +
        ggtitle(plot_titles[soil]) +
        scale_y_continuous(breaks = seq(0, 1, 0.25),
                           limits = c(0, 0.6)) +
        scale_color_manual(values = colors) +
        guides(color = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())
    visnir_plots[[soil]] <- plot
}

visnir_plots_arrange <- ggarrange(visnir_plots[["ox_ufla"]],
                            visnir_plots[["ox_quartz"]],
                            visnir_plots[["glei_ufla"]],
                            nrow = 1, common.legend = T,
                            legend = "bottom")
visnir_plots_arrange

ggsave("visnir_plots.png", visnir_plots_arrange, device = "png",
       width = 210, height = 130, units = "mm", bg = "white")

# Dados COM continuum removal
depth_order <- c("5 cm", "20 cm", "35 cm",
                 "50 cm", "65 cm", "80 cm",
                 "95 cm", "110 cm")

depth <- factor(c(rep("5 cm", 5), rep("20 cm", 5),
                  rep("35 cm", 5), rep("50 cm", 5),
                  rep("65 cm", 5), rep("80 cm", 5),
                  rep("95 cm", 5), rep("110 cm", 5)),
                  levels = depth_order, ordered = T)

visnir_ox_ufla <- read_csv("../Dados/Espectral/visnir_ox_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

visnir_ox_quartz <- read_csv("../Dados/Espectral/visnir_ox_quartz.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

visnir_glei_ufla <- read_csv("../Dados/Espectral/visnir_glei_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Profundidade = depth, .after = "amostra")

# Continuum removal
oldnames <- c(1:2151)
newnames <- c(350:2500)

visnir_ox_ufla_cr <- visnir_ox_ufla %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_ox_ufla$amostra,
               Profundidade = depth, .before = "350")

visnir_ox_quartz_cr <- visnir_ox_quartz %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_ox_quartz$amostra,
               Profundidade = depth, .before = "350")

visnir_glei_ufla_cr <- visnir_glei_ufla %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_glei_ufla$amostra,
               Profundidade = depth, .before = "350")

# Organizando os dados para cria os plots
# agrupa dados por profundidade
visnir_ox_ufla_cr <- visnir_ox_ufla_cr %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

visnir_ox_quartz_cr <- visnir_ox_quartz_cr %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

visnir_glei_ufla_cr <- visnir_glei_ufla_cr %>%
                  group_by(Profundidade) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Banda (nm)",
                               values_to = "Refletância") %>%
                  mutate("Banda (nm)" = as.numeric(`Banda (nm)`))

soil_names <- c("ox_ufla_cr", "ox_quartz_cr", "glei_ufla_cr")
visnir_data <- list(ox_ufla_cr = visnir_ox_ufla_cr,
                        ox_quartz_cr = visnir_ox_quartz_cr,
                        glei_ufla_cr = visnir_glei_ufla_cr)

plot_titles <- list(ox_ufla_cr = "Organossolo Háplico",
                    ox_quartz_cr = "Cambissolo Háplico",
                    glei_ufla_cr = "Gleissolo Háplico")

colors <- brewer.pal(8, "RdGy")

features <- c(480, 600, 1415, 1930, 2205, 2265)
feature_names <- c("Óx. de Fe", "M.O.",
                   "Minerais 2:1/água", "Minerais 2:1/água",
                   "Caulinita", "Gibbsita")
positions <- rep(0, 6)

visnir_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(visnir_data[[soil]],
                   aes(x = `Banda (nm)`, y = `Refletância`)) +
        geom_line(aes(color = Profundidade)) +
        ggtitle(plot_titles[soil]) +
        scale_y_continuous(breaks = seq(0, 1, 0.25),
                           limits = c(0, 1)) +
        scale_color_manual(values = colors) +
        guides(color = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())
    visnir_plots[[soil]] <- plot

        for (i in seq_len(length(features))) {
            plot <- plot +
                geom_vline(linetype = 2, xintercept = features[i]) +
                annotate("text", label = feature_names[i],
                         x = features[i] - 35, y = positions[i], angle = 90,
                         hjust = 0,
                         size = 3,
                        family = "Times New Roman")
        }
    visnir_plots[[soil]] <- plot
}

visnir_cr_plots_arrange <- ggarrange(visnir_plots[["ox_ufla_cr"]],
                            visnir_plots[["ox_quartz_cr"]],
                            visnir_plots[["glei_ufla_cr"]],
                            nrow = 3, common.legend = T,
                            legend = "bottom")
visnir_cr_plots_arrange

ggsave("visnir_cr_plots.png", visnir_cr_plots_arrange, device = "png",
       width = 210, height = 220, units = "mm", bg = "white")
