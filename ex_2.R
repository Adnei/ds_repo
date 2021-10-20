require('ggplot2')
require('fitdistrplus')
require('reshape2')

dataset <- read.table("soja_milho_modificado.csv", sep="|", dec=",", head=T, stringsAsFactors=F)

summary(dataset)


dataset[dataset$soja == min(dataset$milho),]
dataset[dataset$soja == max(dataset$milho),]

dataset[dataset$soja == min(dataset$soja),]
dataset[dataset$soja == max(dataset$soja),]



# ------------    DENSITY ANALYSIS    ------------
# Fitting normal
fit_norm_milho <- fitdist(dataset$milho, "norm")
fit_norm_soja <- fitdist(dataset$soja, "norm")

# Saving pdfs
# --- Milho
pdf(file="milho_dens_norm.pdf")
denscomp(fit_norm_milho, legendtext="Normal")
abline(v=mean(dataset$milho), lty=2, col='blue')
abline(v=median(dataset$milho), lty=2, col='orange')
dev.off()
#pdf(file="")
#cdfcomp(fit_norm_milho, legendtext="Normal")


# --- Soja
pdf(file="soja_dens_norm.pdf")
denscomp(fit_norm_soja, legendtext="Normal")
abline(v=mean(dataset$soja), lty=2, col='blue')
abline(v=median(dataset$soja), lty=2, col='orange')
dev.off()
#------------------------------------------------

# Boxplot
# Melting
dataset.m <- melt(dataset, id.var = c('obs', 'data', 'mes', 'ano'), variable.name='tipo', value.name='valor')
bp_dataset.m <- ggplot(dataset.m, aes(x=tipo, y=valor, fill=tipo)) +
geom_boxplot() +
#geom_point(shape='circle open', color='grey') +
ggtitle('Boxplot - Preço') +
xlab('Produto') +
ylab('Preço') +
theme_bw()
ggsave(filename='boxplot_dataset.pdf', plot=bp_dataset.m, device='pdf', dpi= 400)
