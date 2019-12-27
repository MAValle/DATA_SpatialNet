library(devtools) 
library(oSCR)
library(car) 
library(ggplot2) 
library(ggthemes)

load("~/TIEDENSITY.RData")

rbs.data <- data2oscr(edf = rbs.edf, sess.col = 1,
                      id.col = 2,
                      occ.col = 3,
                      trap.col = 4,
                      sex.col = 5,
                      tdf = list(rbs.tdf1, rbs.tdf2,rbs.tdf3, rbs.tdf4), 
                      K = c(7,5,6,4),
                      ntraps = c(50,50,50,50),
                      trapcov.names = c("jday","jday2"), #covariate names
                      tdf.sep = "/", #char used separator 
                      sex.nacode = "U") #unknown sex code

# CREATE "SF" FILE

rbs.sf <- rbs.data$scrFrame

# Description of "SF"

rbs.sf

# PLOT capture history

par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(0,0,0,0))
plot(rbs.sf,jit = 2)

# Create "STATE SPACE" 

# defines where individuals can live
#􏰀 defines the population of interest
#􏰀 includes unsampled parts of the landscape

rbs.ss <- make.ssDF(scrFrame = rbs.sf, buffer = 4, res=0.5) 
str(rbs.ss)

# BUFFER 4 KM
# RES 0.25 KM


# Plot "STATE SPACE" 

par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(0,0,0,0)) 
plot(ssDF = rbs.ss, scrFrame = rbs.sf)

# Plot "STATE SPACE" AND "TRAPS"

plot(rbs.ss,rbs.sf)



# Fitting model

topmod <- oSCR.fit(list(D~session, p0~b + jday + jday2, sig~session), rbs.sf, rbs.ss)


# Predictions 

# Create data frama
p.pred.df <- data.frame(jday = rep(seq(-1,7,length=100),2),    #obs range
                        jday2 = rep(seq(-1,7,length=100)^2,2), #obs range^2
                        b=0,                                   #pr(initial capture)
                        sex=rep(c(0,1),each=100))              #binary sex

# Predicting detectability

p.preds <- get.real(model = topmod, type = "det", newdata = p.pred.df) 
p.preds$sex <- factor(p.preds$sex)
levels(p.preds$sex) <- c("Female","Male")
head(p.preds)

# Plot detectability

ggplot(p.preds, aes(x=jday*10, y=estimate, fill = sex)) + 
  geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.1, colour=NA) + 
  geom_line(size=1) + 
  facet_grid(.~sex) +
  theme_bw() + 
  scale_color_fivethirtyeight() +
  xlab("Days since Sept 1st")

# Density dataframe

d.pred.df <- data.frame(session = rep(factor(1:4),2)) #session specific

# Predicting density (d.factor = 4, because red = 05)

d.preds <- get.real(model = topmod, type = "dens", newdata = d.pred.df, d.factor = 4) 
d.preds$sex <- factor(d.preds$sex)
levels(d.preds$sex) <- c("Female","Male")
head(d.preds)

# Plot density

ggplot(d.preds, aes(x=session, y=estimate, color = sex, group=sex)) + 
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
  position = position_dodge(width=0.5)) + 
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  theme_bw() + scale_color_fivethirtyeight() + 
  xlab("Session") + ylab("Density (per m^2)")


# DATAFRAME FOR SIGMA

s.pred.df <- data.frame(session = rep(factor(1:4),2), sex = rep(c(0,1),each=4))

# PREDICTING SIGMA
s.preds <- get.real(model = topmod, type = "sig", newdata = s.pred.df) 
s.preds$sex <- factor(s.preds$sex)
levels(s.preds$sex) <- c("Female","Male")
head(s.preds)

#PLOT SIGMA
ggplot(s.preds, aes(x=session, y=estimate, color = sex, group=sex)) + 
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
          position = position_dodge(width=0.5)) + 
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  theme_bw() + scale_color_fivethirtyeight() + 
  xlab("Session")+ ylab("Sigma (m)")

# # TOTAL POPULATION ESTIMATION

# OPTION A:

# Supplying no newdata object, get.real() uses the statespace object

# to compute pixel specific densities.

pixdens <- get.real(topmod, type = "dens") #takes a while

# this returns a list (length 4 = number of sessions) so

# I use the sapply function to apply the columns sums to

# each list item. Note also that I use columns 3, 5, and 6,

# these are the point estimate, and upper and lower intervals:

sapply(pixdens, function(x) apply(x[,c(3,5,6)],2,sum))

# OPTION B:

# Create a newdata object which has one row per session with

# a session ID column (session NOT Session <- this was the error)

pdf <- data.frame(session=factor(c(1:4)))

# USAR ESTE CUANDO ES SOLO UNA SESSION pdf <- data.frame(Session=factor(1))

# Now use get.real() to compute total abundance (NB, every session

# the same number of pixels (757), this might not always be the case).

# TO get total abundance, set N_sex = FALSE:



totalpopulation <-get.real(topmod,
                           
                           type = "dens",
                           
                           newdata = pdf,
                           
                           d.factor = 757,
                           
                           N_sex=FALSE)



# Can also get sex specific estimates of N by setting N_sex = TRUE:



totalpopulationbysexpopulation <-get.real(topmod,
                           
                           type = "dens",
                           
                           newdata = pdf,
                           
                           d.factor = 757,
                           
                           N_sex=TRUE)


# COMPUTING r_max (For estimating the number of links in a giver area)

library("shotGroups")
library("geometry")

# Extracting coordinates from the State Space Grid

coords = cbind(rbs.ss[[1]][["Y"]],rbs.ss[[1]][["X"]])

# Computing the minimum enclosing circle using the Skyum algorithm
# Hi Dear! the object mc contains the center and radius of the minimum enclosing circle.
# "PLATA O PLOMO" "PLATA O PLOMO""PLATA O PLOMO""PLATA O PLOMO""PLATA O PLOMO""PLATA O PLOMO" BAM BAM!

mc <- getMinCircle(coords)

# Plotting the minimum enclosing circle


angles <- seq(0, 2*pi, length.out=200)
circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                mc$ctr[2] + mc$rad*sin(angles))

# determine axis limits so that the circle will be visible
xLims <- mc$ctr[1] + c(-mc$rad, mc$rad)
yLims <- mc$ctr[2] + c(-mc$rad, mc$rad)
plot(coords, xlab="x", ylab="y", xlim=xLims, ylim=yLims, asp=1, type="n")
lines(circ, col="blue", lwd=2)
points(coords, pch=16, cex=1.5)

