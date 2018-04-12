singer = read.table('singer.txt', header = T, sep = ',')
# According to 4 different voice parts, extract the corresponding data of height
alto_height = singer[which(singer$voice.part=='Alto'), 'height']
bass_height = singer[which(singer$voice.part=='Bass'), 'height']
tenor_height = singer[which(singer$voice.part=='Tenor'), 'height']
soprano_height = singer[which(singer$voice.part=='Soprano'), 'height']
# histogram
par(mfrow=c(2,2))
hist(bass_height)
hist(tenor_height)
hist(alto_height)
hist(soprano_height)
# Q-Q plot
par(mfrow=c(2,2))
qqnorm(bass_height, main = 'Bass')
qqline(bass_height)
qqnorm(tenor_height, main = 'Tenor')
qqline(tenor_height)
qqnorm(alto_height, main = 'Alto')
qqline(alto_height)
qqnorm(soprano_height, main = 'Soprano')
qqline(soprano_height)

par(mfrow=c(1,1))
boxplot(bass_height, tenor_height, alto_height, soprano_height, names = c('Bass', 'Tenor', 'Alto', 'Soprano'))

mean(alto_height)
mean(soprano_height)

t.test(alto_height, soprano_height, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE)
