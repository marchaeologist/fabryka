### --- R packages ---

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsci)
library(ggtern)
library(RColorBrewer)
library(circular)
library(CircStats)
library(Ternary)
library(cowplot)
library(RStoolbox)
library(shinyjs)
library(rmarkdown)
library(rsconnect)


### --- Model data from Bertran & Lenoble 2002 ---
# model <- read.csv2("C:/Users/Surface User/Documents/articles/fabriques/fabryka/model.csv")
# ternary_model <- model %>%
#   dplyr::filter(area != "NA")

### --- Model data from Bertran & Lenoble 2002 ---
ternary_model <- data.frame(
  Type = c("Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Snow Avalanche", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Debris Flow", "Mudslides", "Mudslides", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Shallow", "Runoff Steep", "Runoff Steep", "Runoff Steep", "Runoff Steep", "Runoff Steep", "Runoff Steep", "Runoff Steep", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Solifluction", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall", "Rockfall"),
  IS = c(0.303721232519149, 0.0839782209730365, 0.0979745911352092, 0.142762975654162, 0.13856406460551, 0.201547730335288, 0.228140833643416, 0.12316805742712, 0.522565207149734, 0.463565909568313, 0.195259580090895, 0.146093498773044, 0.0828799656500924, 0.0730467493865221, 0.117998595162843, 0.216330757798546, 0.304829704170679, 0.250044642130787, 0.302020213809658, 0.35821002103006, 0.335734098141899, 0.310448684892718, 0.258473113213847, 0.238806680686707, 0.162950440939165, 0.150307734314574, 0.117998595162843, 0.132046046967944, 0.116593849982333, 0.139069772870494, 0.153117224675594, 0.205092796354466, 0.223354483701096, 0.223354483701096, 0.240211425867217, 0.244425661408747, 0.271115819838438, 0.205092796354466, 0.182616873466305, 0.125022321065393, 0.137665027689984, 0.105355888538253, 0.139069772870494, 0.203688051173956, 0.306234449351188, 0.334329352961389, 0.349781549947, 0.36944798247414, 0.36944798247414, 0.324496136697819, 0.404566611986891, 0.420018808972501, 0.441089986680152, 0.107245546488309, 0.128384616098357, 0.123465943513056, 0.0658485032069635, 0.0458587790191352, 0.0317483854747859, 0.0211655903165239, 0.0364518499895691, 0.0658485032069635, 0.103476219325228, 0.0752554322365296, 0.068200235464355, 0.0517381096626142, 0.11405901448349, 0.0376277161182649, 0.0329242516034817, 0.0505622435339184, 0.063496770949572, 0.0258690548313071, 0.047034645147831, 0.0341001177321774, 0.0282207870886986, 0.0188138580591324, 0.0188138580591324, 0.0587933064347888, 0.331222054519825, 0.0598088913604017, 0.0388720899751951, 0.0331249427554835, 0.0228552940996774, 0.0229838283510186, 0.0501224073634603, 0.07244095195059, 0.106468014729378, 0.117006633207437, 0.0429125879687595, 0.0464746529156624, 0.0570680174637374, 0.0460985712172922, 0.0356361211844733, 0.0887648018709795, 0.10394493498082, 0.0815883061710709, 0.0639898249248762, 0.0568990187258619, 0.0638136853952599, 0.0637184748387106, 0.0860036957310481, 0.0951724723267523, 0.135146624494006, 0.159797827716595, 0.157541337526375, 0.165761578952456, 0.137657802922996, 0.172852385151471, 0.228056655970753, 0.218745063540225, 0.159061136035295, 0.12256335931405, 0.140152319504589, 0.602996987778554, 0.490240681505499, 0.394101094051632, 0.154345579660505, 0.061766717667892, 0.0629536261549768, 0.194700468221388, 0.193513559734303, 0.144850311763827, 0.117551416560877, 0.106869240177114, 0.122299050509216, 0.305082957520273, 0.330008035749053, 0.305082957520273, 0.269475702907729, 0.23980299073061, 0.184018291837625, 0.184018291837625, 0.218438637963083, 0.422586897741667, 0.442764342022108, 0.505670491837601, 0.601810079291469, 0.593501719881875, 0.56738973316601, 0.470063237225058, 0.359680747926173, 0.394101094051632, 0.419026172280412, 0.448698884457532, 0.479558505121736, 0.490240681505499, 0.51516575973428, 0.503296674863432),
  PL = c(0.443290898891941, 0.694980586483179, 0.251618765038456, 0.263163966718373, 0.497990694969972, 0.501650377256599, 0.413202310451019, 0.317809910680379, 0.248449756522456, 0.134397093877644, 0.0580879715117297, 0.0717221070611663, 0.31500770573943, 0.473208985404062, 0.631998269328554, 0.681372333996104, 0.593327240372082, 0.631668676501516, 0.518089649786168, 0.492427836200299, 0.463519812242919, 0.405602908161913, 0.532563930011081, 0.520499336055673, 0.62898706663504, 0.609760974691861, 0.596718463975756, 0.480205686978315, 0.447785800069661, 0.437764383637746, 0.436823382796023, 0.441249222260723, 0.447933463745559, 0.434551468611739, 0.416390637431355, 0.436181329879568, 0.347410459910465, 0.39137087676194, 0.282170882001641, 0.308535068177766, 0.286398629707319, 0.245375583711409, 0.199321561253317, 0.246087847892341, 0.244692994302508, 0.245244082643393, 0.201021633785624, 0.160774792217918, 0.144959707059767, 0.165002539923597, 0.29893323901872, 0.310671860720562, 0.21619466602732, 0.123930439019075, 0.101156256684272, 0.663317944740417, 0.683980025382262, 0.654260019859068, 0.641966947792139, 0.472105595880435, 0.415582628976702, 0.390701002979003, 0.472701808870991, 0.645671672883772, 0.623741022797354, 0.61160548692022, 0.652746460171697, 0.599312414853291, 0.577224228577078, 0.505268776400047, 0.479453243853116, 0.177493171158685, 0.538600803698997, 0.362787008343687, 0.259820360019805, 0.145379221683265, 0.0720594660824503, 0.219075851874255, 0.0623825701279208, 0.460556053851353, 0.400936877281269, 0.355069380484035, 0.240389192067524, 0.185502815934599, 0.0968543440910121, 0.0664449772321435, 0.0362933104128667, 0.0361186746993652, 0.175573479399017, 0.15450937045599, 0.130957204167354, 0.31510284136344, 0.282752217146318, 0.0958752083505653, 0.113692545614743, 0.160364542438923, 0.175224207972016, 0.203122624638423, 0.250438871561581, 0.291095446474859, 0.274915880835638, 0.35968771668693, 0.290024739347002, 0.263529287112813, 0.227090112557511, 0.216903075982339, 0.217707576009285, 0.189004659315931, 0.11581431741071, 0.092027343929336, 0.0781095355043048, 0.163299321323156, 0.152505313281391, 0.297047654390442, 0.444908275785636, 0.508396463039311, 0.6745294008151, 0.495710286320978, 0.4077459354259, 0.220581151982326, 0.189309926270602, 0.384271771951782, 0.473985294285182, 0.549223099798293, 0.533285051417979, 0.533375566171118, 0.483908882592548, 0.448060455323148, 0.376437400174318, 0.311098109923821, 0.398608247673714, 0.484951251423468, 0.460545828048259, 0.10047057981149, 0.0914097505730521, 0.123686035575838, 0.121871422429129, 0.201061783964069, 0.245982457277267, 0.288478347837047, 0.415622095611284, 0.388132993530726, 0.360252060889594, 0.33616466868499, 0.32176275125467, 0.330812163687748, 0.299847552341268, 0.379790383705051),
  EL = c(0.25298786858891, 0.221041192543785, 0.650406643826335, 0.594073057627465, 0.363445240424518, 0.296801892408114, 0.358656855905565, 0.559022031892501, 0.228985036327809, 0.402036996554043, 0.746652448397375, 0.78218439416579, 0.602112328610477, 0.453744265209415, 0.250003135508603, 0.10229690820535, 0.10184305545724, 0.118286681367697, 0.179890136404173, 0.149362142769642, 0.200746089615182, 0.283948406945368, 0.208962956775072, 0.24069398325762, 0.208062492425795, 0.239931290993564, 0.285282940861401, 0.387748266053741, 0.435620349948006, 0.42316584349176, 0.410059392528383, 0.353657981384811, 0.328712052553345, 0.342094047687165, 0.343397936701428, 0.319393008711685, 0.381473720251097, 0.403536326883594, 0.535212244532054, 0.566442610756841, 0.575936342602697, 0.649268527750338, 0.661608665876189, 0.550224100933704, 0.449072556346304, 0.420426564395218, 0.449196816267376, 0.469777225307942, 0.485592310466093, 0.510501323378584, 0.296500148994389, 0.269309330306937, 0.342715347292528, 0.768824014492616, 0.770459127217371, 0.213216111746527, 0.250171471410775, 0.299881201121797, 0.326284666733075, 0.506728813803041, 0.547965521033729, 0.543450493814034, 0.423821971803781, 0.279072894879699, 0.308058741738291, 0.336656403417166, 0.233194525344813, 0.363059869028444, 0.38985151981944, 0.444168980066035, 0.457049985197312, 0.796637774010008, 0.414364551153172, 0.603112873924135, 0.711958852891496, 0.835806920257603, 0.909126675858417, 0.722130841690956, 0.606395375352254, 0.479635054788245, 0.560191032743536, 0.611805676760481, 0.736755513832798, 0.791513355714383, 0.853023248545528, 0.861114070817267, 0.857238674857755, 0.846874692093197, 0.781513932632223, 0.799015976628348, 0.811974778368909, 0.638798587419268, 0.681611661669209, 0.815359989778455, 0.782362519404436, 0.758047151390006, 0.760785967103108, 0.739978356635715, 0.685747443043159, 0.64518607868643, 0.639080423433314, 0.545139810986317, 0.574828636158992, 0.576672885170591, 0.615368549916114, 0.617335345065205, 0.644634621067719, 0.638142955532598, 0.656129026618536, 0.689227592530439, 0.762829328460401, 0.714137319362794, 0.70734236721402, 0.0999553578310047, 0.0648510427088648, 0.0975024429090569, 0.171125019524395, 0.44252299601113, 0.529300438419124, 0.584718379796286, 0.617176513995095, 0.470877916284391, 0.408463289153941, 0.343907660024594, 0.344415898072805, 0.161541476308609, 0.186083081658399, 0.24685658715658, 0.354086896917953, 0.44909889934557, 0.417373460488661, 0.331030456738908, 0.321015533988658, 0.476942522446843, 0.46582590740484, 0.370643472586561, 0.276318498279402, 0.205436496154056, 0.186627809556723, 0.241458414937895, 0.224697156462543, 0.217765912417642, 0.220721766829993, 0.215136446857479, 0.198678743623594, 0.178947154806753, 0.184986687924453, 0.116912941431517),
  area = c(1, 1, 1, 1, NA, NA, NA, NA, 2, 2, 2, 2, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, 3, 3, 3, 3, 3, 3, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 6, 6, 6, 6, 6, 6, NA, 6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 6, 6, NA, 6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
) %>%
  dplyr::filter(area != "NA")

### --- Mcpherron (2018) functions not modified ---
### Some functions taken from McPherron (2018) are modified later on in the code

#################################################################################################################
# Compute plunge and bearing angles from XYZ 2-shot data

plunge_and_bearing <- function(xyz) {
  # Compute the plunge angle
  # Note: Angles are converted to degrees
  run <- sqrt((xyz$X2 - xyz$X1)^2 + (xyz$Y2 - xyz$Y1)^2)
  rise <- abs(xyz$Z2 - xyz$Z1)
  plunge_angle <- ifelse(run == 0, 90, deg(atan(rise / run)))

  # Compute Schmidt bearing (lower hemisphere)
  # Note: Angle are adjusted to be clockwise with north (0) = positive y-axis
  # Note: Angles are converted to degrees.
  run <- ifelse(xyz$Z1 >= xyz$Z2, xyz$X2 - xyz$X1, xyz$X1 - xyz$X2)
  rise <- ifelse(xyz$Z1 >= xyz$Z2, xyz$Y2 - xyz$Y1, xyz$Y1 - xyz$Y2)
  slope <- ifelse(run != 0, rise / run, 1000000)
  bearing_angle <- 90 - deg(atan(slope))
  bearing_angle[run <= 0] <- 180 + bearing_angle[run <= 0]

  return(data.frame(plunge = plunge_angle, bearing = bearing_angle))
}

########################################################################################################
# convert to radians

rad <- function(deg) {
  (deg * pi) / (180)
}

#################################################################################################################
# Convert radians to degrees
#
deg <- function(radian) {
  (radian * 180) / pi
}

#################################################################################################################
# Return vector(artifact) lengths

compute_lengths <- function(xyz) {
  return(sqrt((xyz$X1 - xyz$X2)^2 + (xyz$Y1 - xyz$Y2)^2 + (xyz$Z1 - xyz$Z2)^2))
}

#################################################################################################################
# Helping function to place points on Benn diagram
# Expects an object with columns named elongation and isotropy

benn_coords <- function(benn) {
  return(cbind(X = benn[, "elongation"] + (.5 * benn[, "isotropy"]), Y = benn[, "isotropy"] * .866))
}

#################################################################################################################
#  Normalize vectors

vector_normals <- function(xyz) {
  l <- compute_lengths(xyz)
  xnorm <- ifelse(l != 0, (xyz$X1 - xyz$X2) / l, 0)
  ynorm <- ifelse(l != 0, (xyz$Y1 - xyz$Y2) / l, 0)
  znorm <- ifelse(l != 0, (xyz$Z1 - xyz$Z2) / l, 0)

  return(cbind(xnorm, ynorm, znorm))
}

#################################################################################################################
#  Compute eigen values from normalized vectors

eigen_values <- function(xyz) {
  l <- xyz[, 1] # X
  m <- xyz[, 2] # Y
  n <- xyz[, 3] # Z

  # Build a matrix prior to computing eigen values
  M11 <- sum(l^2)
  M12 <- sum(l * m)
  M13 <- sum(l * n)
  M21 <- sum(m * l)
  M22 <- sum(m^2)
  M23 <- sum(m * n)
  M31 <- sum(n * l)
  M32 <- sum(n * m)
  M33 <- sum(n^2)
  M <- matrix(c(M11, M12, M13, M21, M22, M23, M31, M32, M33), nrow = 3, ncol = 3)

  # Compute eigen values on matrix normalized for sample size
  n <- nrow(xyz)
  return(eigen(M / n))
}

###############################################################################################################
# Rose.diag from CircStats package adapted to do only plunge angle from 0 to 90,
# to deal with angles that turn clockwise, and some other customizations.

rose_diagram_plunge <- function(x, bins = 10, main = "", prop = 1,
                                cex = 1, pch = 19, pts_on_edge = FALSE,
                                color_codes = NULL, color_filled = NULL,
                                pnt_col = "black", bar_col = "white", bg = "white",
                                dotsep = 40, shrink = 1, ...) {
  x <- rad(x)
  x <- x %% (2 * pi)
  plot(cos(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000)),
    sin(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000)),
    axes = FALSE, xlab = "", ylab = "", main = "", type = "l",
    xlim = shrink * c(-.15, 1.15), ylim = shrink * c(-1.15, 0.15), asp = 1
  )
  polygon(c(0, cos(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000))),
    c(0, sin(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000))),
    col = bg
  )
  # text(-.88,1.05,main,cex=1.1)
  text(-.03, .1, main, cex = cex * 1.1)
  lines(c(0, 0), c(-0.9, -1))
  text(0.005, -1.075, "90", cex = cex * 1.1)
  lines(c(0.9, 1), c(0, 0))
  text(1.05, 0, "0", cex = cex * 1.1)
  lines(c(0, 0), c(-1, 0))
  n <- length(x)
  freq <- c(1:bins)
  arc <- (1 / 2 * pi) / bins
  for (i in 1:bins) {
    newi <- bins - i + 1 # This turns the angles clockwise
    freq[i] <- sum(x <= newi * arc & x > (newi - 1) * arc)
  }
  rel.freq <- freq / n
  radius <- sqrt(rel.freq) * prop
  # radius <- freq/max(freq) * prop     					  # This will bring bars to circle but with flat proportions
  radius <- radius / max(radius) * prop # This will bring bars to circle but with exponential proportions
  sector <- seq(0, 1 / 2 * pi - (1 / 2 * pi) / bins, length = bins)
  sector <- sector - (pi / 2) # This rotates them to the right spot
  mids <- seq(arc / 2, 1 / 2 * pi - (pi / 4) / bins, length = bins)
  index <- cex / dotsep
  for (i in 1:bins) {
    if (rel.freq[i] != 0) {
      xp <- c(0, radius[i] * cos(sector[i]), radius[i] * cos(sector[i] + (1 / 2 * pi) / bins))
      yp <- c(0, radius[i] * sin(sector[i]), radius[i] * sin(sector[i] + (1 / 2 * pi) / bins))
      polygon(xp, yp, col = bar_col, ...)

      if (pts_on_edge) {
        xp <- cos(x)
        yp <- -sin(x)
        if (!is.null(color_codes)) {
          points(xp[color_filled], yp[color_filled],
            cex = (cex * .8),
            col = color_codes[color_filled], pch = 19
          )
          points(xp[!color_filled], yp[!color_filled],
            cex = (cex * .8),
            col = color_codes[!color_filled], pch = 21
          )
        } else {
          points(xp, yp, cex = (cex * .8), pch = pch, col = pnt_col)
        }
      }
    }
  }
}

#################################################################################################################
# Function to plot points on circle in Schmidt equal area space
# Code to draw circle taken from rose.diag in CircStats package
# Angles should be in decimal degrees

schmidt_diagram <- function(bearing = NULL, plunge = NULL, angles = NULL,
                            level = "All Points", color_codes = NULL,
                            color_filled = FALSE, redraw = TRUE, col = "black",
                            pch = 19, cex = 1, main = "", ...) {
  library(CircStats)

  if (is.null(angles) & (is.null(bearing) | is.null(plunge))) stop("Not enough data passed to schmidt.diagram.2shot. Bearing and plunge angles are required.")

  if (!is.null(angles)) {
    plunge <- angles[, 1]
    bearing <- angles[, 2]
  }

  level <- factor(level)

  for (l in levels(level)) {
    bearing_angle_level <- subset(bearing, level == l)
    plunge_angle_level <- subset(plunge, level == l)

    # If the circle doesn't already exist (from doing a Rose diagram), then make it
    if (length(levels(level)) > 1) {
      if (!exists("main")) draw_circle_diagram(main = l, ...) else draw_circle_diagram(main = main[which(levels(level) == l)], ...)
    } else {
      if (redraw) draw_circle_diagram(...)
    }

    # Shift points into Schmidt space
    d <- sin(rad((90 - plunge_angle_level) / 2)) / sin(rad(45))
    x <- d * sin(rad(bearing_angle_level))
    y <- d * cos(rad(bearing_angle_level))

    # Plot them using color coding or not
    if (!is.null(color_codes)) {
      points(x[color_filled], y[color_filled],
        cex = (cex * .8),
        col = color_codes[color_filled], pch = 19
      )
      points(x[!color_filled], y[!color_filled],
        cex = (cex * .8),
        col = color_codes[!color_filled], pch = 21
      )
    } else {
      points(x, y, cex = (cex * .8), pch = pch, col = col)
    }
  }
}

#################################################################################################################
# Internal function to setup a circular graph for rose or schmidt diagrams
#
draw_circle_diagram <- function(bg = "white", cex = 1, main = "", ...) {
  plot(cos(seq(0, 2 * pi, length = 1000)), sin(seq(0, 2 * pi, length = 1000)),
    axes = FALSE,
    xlab = "", ylab = "", main = "", type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1
  )
  polygon(cos(seq(0, 2 * pi, length = 1000)), sin(seq(0, 2 * pi, length = 1000)), col = bg)

  text(-.88, 1.05, main, cex = (cex * 1.1))
  lines(c(0, 0), c(0.9, 1))
  text(0.005, 1.10, "0", cex = (cex * 1.1))
  lines(c(0, 0), c(-0.9, -1))
  text(0.005, -1.11, "180", cex = (cex * 1.1))
  lines(c(-1, -0.9), c(0, 0))
  text(-1.16, 0, "270", cex = (cex * 1.1))
  lines(c(0.9, 1), c(0, 0))
  text(1.13, 0, "90", cex = (cex * 1.1))
  lines(c(-.05, .05), c(0, 0))
  lines(c(0, 0), c(-0.05, .05))
}


#################################################################################################################

### --- Application ---
### --- ui ---

ui <- tagList(
  useShinyjs(),
  navbarPage(
    p(strong("Fabryka"), "\\(\\beta\\) 1.0"),
    tabPanel(
      "Presentation",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka4.png", height = 200),
          tags$br()
        ),
        column(12,
          align = "justify",
          h2("Presentation"),
          hr(),
          h5("Fabryka is a R shiny application dedicated to the fabric analysis of archaeological remains. This is the \\(\\beta\\)-test version 1.0. The application allows a deep exploration of the fabric data. An article, a tutorial video and the source code are available via the links respectively", strong("here,"), strong("here"), "and", strong("here."), "If you use fabryka, please cite the article."),
          tags$br(),
          h5(strong("The 'Upload your data'"), "panel allows the user to upload different data sources."),
          tags$br(),
          h5(strong("The 'Classical method'"), "panel contains subpanels to perform:"),
          tags$li("Benn diagrams with possible integration of Bertran & Lenoble model (2002)"),
          tags$li("Rose diagrams for bearing and plunge"),
          tags$li("Schmidt diagrams"),
          tags$li("Summary data table with orientation statistical tests."),
          tags$br(),
          h5(strong("The 'Spatialised method'"), "panel contains subpanels to perform:"),
          tags$li("Benn diagrams with Benn indices computed with nearest neighbors points (McPherron 2018)"),
          tags$li("Spatial projections of the points according to their fabric shape"),
          tags$li("Summary data tables with orientation statistical tests."),
          tags$br(),
          h5(strong("The 'Spatial exploration'"), "panel contains subpanels to perform:"),
          tags$li("A spatial exploration of your data starting from a Benn diagram"),
          tags$li("A spatial exploration of your data starting from the spatial projections"),
          tags$li("Summary data tables with orientation statistical tests."),
          tags$br(),
          h5(strong("The 'Markdown report'"), "panel allows to download automatic reports of your data."),
          tags$br(),
          h5(strong("The 'Tutorial'"), "panel contains a summary of the mathematics, statistics and methods used in Fabryka and a short tutorial."),
          tags$br(),
          h5(strong("The 'New models'"), "panel is in progress. It will contain new Benn diagram models of natural processes for the spatialised method."),
          tags$br(),
          h5(strong("The 'Bibliography'"), "panel contains the bibliography cited in the application."),
          tags$br(),
          hr(),
          tags$br()
        )
      )
    ),
    tabPanel(
      "Upload your data",
      sidebarPanel(
        fileInput("user_data", "Upload your data file (.csv)", multiple = FALSE, accept = ".csv"),
        tags$br(),
        radioButtons("sep", "Column separator",
          choices = c(Semicolon = ";", Comma = ",", Tab = "\t"),
          selected = ";",
          inline = TRUE
        ),
        tags$br(),
        radioButtons("decimal", "Decimal separator",
          choices = c(Comma = ",", Point = "."),
          selected = ",",
          inline = TRUE
        ),
        hr(),
        radioButtons("data_type",
          label = "What form does your dataset take?",
          choices = c(
            "Case 1: Only angles",
            "Case 2: Only angles from DistoX2",
            "Case 3: Angles from DistoX2 with coordinates",
            "Case 4: Angles and coordinates",
            "Case 5: Two shots data without angles"
          )
        ),
        hr(),
        h5(strong("Example files for each case via the links below")),
        downloadLink("angles_only", "Case 1: Only angles"),
        tags$br(),
        downloadLink("angles_only_disto", "Case 2: Only angles from DistoX2"),
        tags$br(),
        downloadLink("disto_one_shot", "Case 3: Angles from DistoX2 with coordinates"),
        tags$br(),
        downloadLink("angles_one_shot", "Case 4: Angles and coordinates"),
        tags$br(),
        downloadLink("two_shots", "Case 5: Two shots data without angles"),
        tags$br(),
      ),
      mainPanel(
        h5(strong("View of the first line of your dataset")),
        tableOutput("user_data_head"),
        hr(),
        h5(strong("Depending on the source and form of your data, select the appropriate columns.")),
        h5(strong("In any case,"), "whatever the form of your dataset you can select your corresponding context columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("id1")
          ),
          column(
            4,
            uiOutput("code1")
          ),
          column(
            4,
            uiOutput("sample1")
          )
        ),
        tags$br(),
        h5("In", strong("cases 1, 2, 3"), "and", strong("4,"), "you must select your 'direction' and 'plunge' columns. If your direction and plunge come from a DistoX2, they will be converted to classical angles (by checking cases 2 or 3)."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("direction1")
          ),
          column(
            4,
            uiOutput("dip1")
          ),
          column(
            4,
          )
        ),
        tags$br(),
        h5("In", strong("cases 3,"), strong("4"), "and", strong("5,"), "you must select your x, y and z coordinates columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("x1")
          ),
          column(
            4,
            uiOutput("y1")
          ),
          column(
            4,
            uiOutput("z1")
          )
        ),
        tags$br(),
        h5("In", strong("case 5,"), "you must select your X2, Y2 and Z2 coordinates columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("x2")
          ),
          column(
            4,
            uiOutput("y2")
          ),
          column(
            4,
            uiOutput("z2")
          )
        ),
        hr(),
        h5(strong("View of the data that will be used in the application")),
        tags$br(),
        DT::dataTableOutput("app_data_view"),
        hr(),
        tags$br()
      )
    ), tabPanel(
      "Classical method",
      sidebarPanel(
        h5(strong("About your data")),
        fluidRow(
          column(
            6,
            checkboxInput("plot_all_samples_c", strong("Plot all samples"), TRUE),
            uiOutput("sample_c")
          ),
          column(
            6,
            checkboxInput("plot_all_codes_c", strong("No code distinction"), TRUE),
            uiOutput("code_c")
          )
        ),
        hr(),
        sliderInput("minimum_n",
          "Minimum number of measures in samples to compute Benn diagram and summary table statistics",
          min = 10,
          max = 60,
          value = 40
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Benn diagram",
            h5(strong("Benn diagram")),
            plotOutput("benn"),
            hr(),
            checkboxGroupInput("model", "Build your model",
              choices = c("none", unique(ternary_model$Type)),
              selected = "none",
              inline = TRUE
            ),
            tags$br(),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the Download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_benn_c",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                )
              ),
              column(
                6,
                downloadButton("download_benn_c", "Benn diagram")
              )
            ),
            hr(),
            tags$br(),
          ),
          tabPanel(
            "Rose diagrams",
            h5(strong("Rose diagrams")),
            splitLayout(
              style = "border: 1px solid silver:",
              cellWidths = c(400, 400),
              plotOutput("orientation_rose_c"),
              plotOutput("plunge_rose_c")
            ),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Orientation Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_orientation",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_orientation_rose_c", "Orientation rose diagram")
              ),
              column(
                6,
                # h5(strong("Plunge Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_plunge",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_plunge_rose_c", "Plunge rose diagram")
              )
            ),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Schmidt diagram",
            h5(strong("Schmidt diagram")),
            fluidRow(
              column(10,
                align = "center",
                plotOutput("schmidt_diagram_c")
              )
            ),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_schmidt_c",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                )
              ),
              column(
                6,
                downloadButton("download_schmidt_c", "Schmidt diagram")
              )
            ),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Summary table",
            h5(strong("Summary table of samples")), tags$br(),
            DT::dataTableOutput("summary_table_c"), tags$br(),
            hr(),
            tags$br()
          )
        )
      )
    ), tabPanel(
      "Spatialised method",
      sidebarPanel(
        h5(strong("About your data")),
        fluidRow(
          column(
            6,
            # checkboxInput("plot_all_samples_sm",
            #               strong("Plot all samples"), TRUE),
            tags$br(),
            tags$br(),
            uiOutput("sample_sm")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_sm",
              strong("No code distinction"), TRUE
            ),
            uiOutput("code_sm")
          )
        ),
        hr(),
        sliderInput("n_nearest_sm",
          "Number of nearest points in spatial series",
          min = 10,
          max = 60,
          value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_sm",
          strong("Uncheck this box to make sticks in the projections"), TRUE
        ),
        hr(),
        fileInput("ortho_xy_sm",
          "Upload XY ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_sm",
          "Upload XZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_sm",
          "Upload YZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_yz_ortho",
          strong("Include YZ ortho"), FALSE
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Benn diagram & projections",
            h5(strong("Benn diagram")),
            plotOutput("benn_sm"),
            h5(strong("XY projection")),
            plotOutput("projection_xy_sm", inline = T),
            h5(strong("XZ projection")),
            plotOutput("projection_xz_sm", inline = T),
            h5(strong("YZ projection")),
            plotOutput("projection_yz_sm", inline = T),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Benn diagram")),
                radioButtons(
                  inputId = "file_format_benn_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_sm", "Benn diagram"),
                hr(),
                # h5(strong("XY projection")),
                radioButtons(
                  inputId = "file_format_xy_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xy_sm", "XY projection"),
                hr(),
              ),
              column(
                6,
                # h5(strong("XZ projection")),
                radioButtons(
                  inputId = "file_format_xz_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xz_sm", "XZ projection"),
                hr(),
                # h5(strong("YZ projection")),
                radioButtons(
                  inputId = "file_format_yz_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_yz_sm", "YZ projection"),
                hr(),
              )
            )
          ),
          tabPanel(
            "Spatial series data table",
            h5(strong("Statistics table of spatial series")), tags$br(),
            DT::dataTableOutput("summary_table_sm"),
            tags$br(),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Summary table of samples",
            h5(strong("Summary table of samples")), tags$br(),
            DT::dataTableOutput("summary_table2_sm"),
            tags$br(),
            hr(),
            tags$br()
          )
        )
      )
    ), tabPanel(
      "Spatial exploration",
      sidebarPanel(
        fluidRow(
          column(
            6,
            checkboxInput(
              "plot_all_samples_se",
              strong("No sample distinction"), TRUE
            ),
            uiOutput("sample_se")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_se",
              strong("No code distinction"), TRUE
            ),
            uiOutput("code_se")
          )
        ),
        hr(),
        sliderInput("n_nearest_se",
          "Number of nearest points in spatial series in the interactive Benn diagram",
          min = 10,
          max = 60,
          value = 40
        ),
        sliderInput("n_nearest_se2",
          "Number of nearest points in the spatial series for the new Benn diagram built with the selected points",
          min = 10,
          max = 60,
          value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_se",
          strong("Uncheck this box to make sticks in the projections"), TRUE
        ),
        hr(),
        fileInput("ortho_xy_se",
          "Upload XY ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho_se",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_se",
          "Upload XZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho_se",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_se",
          "Upload YZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_yz_ortho_se",
          strong("Include YZ ortho"), FALSE
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "From Benn diagram",
            h5(strong("Interactive Benn diagram & new plots")),
            h5("Use the lasso at the top right of the Benn diagram to sample points."),
            plotlyOutput("interactive_ternary"),
            hr(),
            h5(strong("New analysis with the selected points (i.e. spatial series)")),
            tags$br(),
            h5(strong("Summary table of selected points")),
            DT::dataTableOutput("selected_sample_summary"),
            tags$br(),
            fluidRow(
              column(
                6,
                h5(strong("Benn diagram")),
                plotOutput("selected_benn")
              ),
              column(
                6,
                h5(strong("Schmidt diagram")),
                plotOutput("schmidt_selected")
              )
            ),
            fluidRow(
              column(
                6,
                h5(strong("Orientation rose diagram")),
                plotOutput("orientation_rose_selected")
              ),
              column(
                6,
                h5(strong("Plunge rose diagram")),
                plotOutput("plunge_rose_selected")
              )
            ),
            h5(strong("XY projection")),
            plotOutput("projection_xy_selected_se", inline = T),
            h5(strong("XZ projection")),
            plotOutput("projection_xz_selected_se", inline = T),
            h5(strong("YZ projection")),
            plotOutput("projection_yz_selected_se", inline = T),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Benn diagram")),
                # hr(),
                radioButtons(
                  inputId = "file_format_benn_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_se1", "Benn diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_schmidt_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_schmidt_se1", "Schmidt diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_o_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_o_se1", "Orientation rose diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_p_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_p_se1", "Plunge rose diagram"),
                hr()
              ),
              column(
                6,
                # h5(strong("XY projection")),
                radioButtons(
                  inputId = "file_format_xy_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xy_se1", "XY projection"),
                hr(),
                # h5(strong("XZ projection")),
                radioButtons(
                  inputId = "file_format_xz_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xz_se1", "XZ projection"),
                hr(),
                # h5(strong("YZ projection")),
                radioButtons(
                  inputId = "file_format_yz_se1",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_yz_se1", "YZ projection"),
                hr()
              )
            ),
          ),
          tabPanel(
            "Get selected data from Benn diagram",
            h5(strong("Statistics table of selected spatial series")),
            tags$br(),
            DT::dataTableOutput("click"),
            tags$br(),
            hr(),
            tags$br()
          ),
          tabPanel(
            "From projection",
            fluidRow(
              column(
                6,
                h5(strong("Interactive projections & new plots")),
                h5("Use your mouse to sample points in the projection.")
              ),
              column(
                6,
                tags$br(),
                radioButtons("axis",
                  "Choose what axis you want to plot",
                  choices = c("XY", "XZ", "YZ"),
                  inline = TRUE
                ),
                tags$br(),
              )
            ),
            plotOutput("interactive_projection",
              brush = brushOpts(
                id = "plot1_brush"
              )
            ),
            # plotlyOutput("interactive_projection"),
            hr(),
            h5(strong("New analysis with the selected points (i.e. spatial series)")),
            tags$br(),
            h5(strong("Summary table of selected points")),
            DT::dataTableOutput("selected_sample_summary2"),
            tags$br(),
            fluidRow(
              column(
                6,
                h5(strong("Benn diagram")),
                plotOutput("selected_benn2")
              ),
              column(
                6,
                h5(strong("Schmidt diagram")),
                plotOutput("schmidt_selected2")
              )
            ),
            fluidRow(
              column(
                6,
                h5(strong("Orientation rose diagram")),
                plotOutput("orientation_rose_selected2")
              ),
              column(
                6,
                h5(strong("Plunge rose diagram")),
                plotOutput("plunge_rose_selected2")
              )
            ),
            hr(),
            # column(6,
            #        plotOutput("projection_xy_selected_se", inline = T),
            #        plotOutput("projection_xz_selected_se", inline = T),
            #        plotOutput("projection_yz_selected_se", inline = T)
            # )
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Benn diagram")),
                radioButtons(
                  inputId = "file_format_benn_se2",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_se2", "Download Benn diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_schmidt_se2",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_schmidt_se2", "Download Schmidt diagram"),
                hr()
              ),
              column(
                6,
                radioButtons(
                  inputId = "file_format_rose_o_se2",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_o_se2", "Download orientation rose diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_p_se2",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_p_se2", "Download plunge rose diagram"),
                hr(),
              )
            )
          ),
          tabPanel(
            "Get selected data form projection",
            h5(strong("Summary data of selected sample")),
            tags$br(),
            DT::dataTableOutput("click2"),
            tags$br(),
            hr(),
            tags$br()
          )
        )
      )
    ),
    tabPanel(
      "Markdown report",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka4.png", height = 200),
          tags$br()
        ),
        column(12,
          align = "justify",
          h2("Markdown report"),
          hr(),
          h5("Work in progress. This option will be available in the next version."),
          h5("To download a rmarkdown report with figures and summary statistics of your data, choose a file format and then push the", strong("'Generate report'"), "button."),
          tags$br(),
          column(
            12,
            align = "center",
            # sliderInput("slider", "Slider", 1, 100, 50),
            tags$br(),
            radioButtons("format", "Document format", c("HTML", "PDF", "Word"),
              inline = TRUE
            ),
            downloadButton("report", "Generate report")
          ),
          hr(),
          tags$br()
          # disabled(
          #   actionButton("render_report_button","Render Report",width = "100%")
          # ),
          # disabled(
          #   downloadButton("download_rendered_report", "Download Report", style = "width:100%;")
          # )
        )
      )
    ),
    tabPanel(
      "Tutorial",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka4.png", height = 200),
          tags$br()
        ),
        column(12,
          align = "justify",
          h2("Tutorial"),
          hr(),
          h4(strong("Uploading your data")),
          tags$br(),
          h5("First, upload your .csv file in the left sidebar panel. Pay attention to your decimal and column delimiters."),
          tags$br(),
          h5("Fabryla supports three angle measurement methods:"),
          tags$ul(
            tags$li("angles taken with a compass and an inclinometer"),
            tags$li("angles taken with a DistoX2"),
            tags$li("angles taken with two shots at the total station.")
          ),
          tags$br(),
          h5("Five cases (i.e. forms of data) are covered by the application:"),
          tags$ul(
            tags$li("check", strong("'Case 1: Only angles'"), "if you have in your dataset the columns 'id', 'sample', 'code', direction' and 'dip'."),
            tags$li("check", strong("'Case 2: Only angles from DistoX2'"), "if you have in your dataset the same columns that above but your data comes from a DistoX2 and needs to be converted."),
            tags$li("check", strong("'Case 3: Angles from DistoX2 with coordinates'"), "if you have in your dataset the same columns that above (Case 2, data from DistoX2) with a single set of coordinates (x, y, z)."),
            tags$li("check", strong("'Case 4: Angles and coordinates'"), "if your data has the same form that above (Case 3) but is coming from a compass and a clinometer."),
            tags$li("check", strong("'Case 5: Two shots data without angles'"), "if you have the columns 'id', 'sample', 'code' and the angles have been measured with a total station (i.e. two shots data).")
          ),
          tags$br(),
          p("If you miss a context column, please create it by repeating a same character."),
          tags$br(),
          h5(style = "text-align: justify;", "According to your method of angle measurement and the presence of coordinates you have to select the right case.
           Please refer to the example files in the left side bar panel. It contains a dataset coming from Le Moustier Lower shelter (Dordogne, France).
           The exmample files contain directly the good column names.
           You can use them directly in the application.
           Whatever the names of the columns in your file, you can select the appropriate columns in the column selector.
           Please keep only complete lines (i.e. no empty modalities, so no line without a fabric measurement).
           When a data table with new columns appears below the column selector, you are ready to use the application.
          "),
          tags$br(),
          h5(style = "text-align: justify;", "If your data comes from a DistoX2, the columns 'direction' (= 'bearing', 0 to 360° angles) and 'dip' (= 'plunge') are automatically corrected in the new table.
           Whatever the form of your dataset, two new columns 'orientation_pi' (0 to 180° angles) and 'angle_double' (see Krumbein 1939) appear.
           If the angles were measured using a total station (i.e. two shots data), the bearing and the plunge are also calculated.
           If only one set of coordinates is provided with angles, a second set of coordinates is calculated and displayed in the new table.
           You can download the new data by pushing the 'copy', the 'csv' or the 'pdf' buttons."),
          tags$br(),
          hr(),
          h4(strong("Classical method")),
          tags$br(),
          h5(style = "text-align: justify;", "The classical method is available in all cases (i.e. source of data and absence or presence of coordinates).
           You can control the data you plot and the data appearing in the figures and the summary data table by changing the checkboxes filters (samples and code) and the slider (i.e. minimum number of measures) in the left sidebar panel.
           Certain geological processes can have a different impact on objects depending on their physical properties (e.g. silex versus bones in a water flow as in the example files provided where bones have a strong preferential orientation and are much more 'linear' in the Benn diagram, see Thomas et al., 2019, Texier et al., 2020).
           The natural processes models that you can add to the Benn diagram are calculated using data from Bertran & Lenoble (2002).
           In the rose diagram, orientations are considered (and not directions) as the Rayleigh test is perform on orientations (see Curray 1956) in the summary table.
           You can download all the figures by selecting a file format and pressing the 'download' button.
           "),
          tags$br(),
          h5("In this last table:"),
          tags$ul(
            tags$li("'n' stands for number of measures in the samples"),
            tags$li("'E1', 'E2' and 'E3' are the eigenvalues (see Woodcock 1977). They correspond to the sum of the cosines formed by the axis of the objects and the three axis of the space. They are used to compute the Benn indices."),
            tags$li("'IS', 'PL' and 'EL' are calculated thanks to the eigenvalues (Benn 1994). These are the Benn indices, ratios standing respectively for, 'isotropy' with IS = E3/E1, 'elongation' with EL = 1-(E2/E1) and 'planar' PL = 1-IS-EL"),
            tags$li("'L' is the vector magnitude (strength of the preferred artefact orientation) and 'R.p' is the p-value result of the Rayleigh test."),
            tags$li("'L.double' and 'R.p.double' are the same statistics that above for double angles (see Krumbein 1939)."),
          ),
          tags$br(),
          p("Curray's formula to calculate 'L' is", strong("(1.)")),
          h5(style = "text-align: left;", withMathJax("$$L = \\frac{r*100}{n}$$")),
          p("with", strong("(2.)")),
          h5(withMathJax("$$r = \\sqrt{\\sum_{i=\\alpha}^n sin{(2\\alpha)} + \\sum_{i=\\alpha}^n cos{(2\\alpha)}}$$")),
          p("where 'n' is the number of measures and '\\(\\alpha\\)', is the orientation (0 to 180°)"),
          tags$br(),
          p("The Rayleigh test is perform thanks to the formula", strong("(3.)")),
          h5(withMathJax("$$p = e^{(-(n*L^2))*10^-4}$$")),
          tags$br(),
          hr(),
          h4(strong("Spatialised method")),
          tags$br(),
          p(style = "text-align: justify;", "In order to distinguish different statistical populations, a spatial analysis method proposed by McPherron (2018) is used.
        Thanks to this methods it is possible to calculate the Benn indices and to test the orientation rate L (formula (3.) modified R McPherron 2018 script computed for orientations and double angles) for each remains, taking into account its n nearest neighbors in 3 dimensions (R McPherron 2018 script, modified , formula (4.)).
        The analysis of the fabric as proposed by McPherron (2018) has the advantage of spatializing the measurements and thus avoiding the arbitrary sampling classically carried out by square or by décapage.
        It is possible to localize the statistical signal of a series by varying the number of measurements sampled to calculate the Benn indices.
        The choice of about 40 to 50 measurements (each vestige and its 40 to 50 closest neighbors) to calculate the Benn indices allows local particularities to be observed, and above all, it allows the Rayleigh test to be carried out in its field of application (stability of the test between 40 and 50 measurements).
        Several modes of representation are used in the 'Benn diagram & projections' subpanel."),
          tags$br(),
          p(style = "text-align: justify;", "The Benn indices (1994) calculated for each series of remains are projected within a ternary diagram in which each pole (isotropic, linear and planar) is colored respectively in red, green and blue.
        The RGB color code is reused to materialize the fabric of the remains on spatial projections of the material along the axes of the excavation grid.
        Projections (xy, xz and yz axes) are performed. On the latter, the orientation of the objects can be materialized by sticks (by uncecking the corresponding box in the left sidebar panel) which color is representative of the fabric of the series formed by the vestige and its 50 closest neighbors.
        In the left sidebar panel, you can control the samples taken into account in the analyses by selecting the sample explored or by distinguishing the code used (e.g. flint versus bone).
        You can also use the slider to change the number of neighbours searched for around each point.
        Be careful, this will alter the results obtained in the Benn diagram and Rayleigh tests.
          "),
          tags$br(),
          p(style = "text-align: justify;", "At the spatial series scale (i.e. each points and n neighbors), statistics are provided in a table (Spatial series data table subpanel) with the Benn indices, the orientation rates L, L.double and p-values returned by the Rayleigh tests.
        At the sample scale, mean and standard deviations values of Benn indices, mean L and its corresponding Rayleigh tests means (mean values of all spatial series in the sample) and rejections ratios 'Tr' (formula 4.) are given in the subpanel 'Summary table of samples'."),
          tags$br(),
          p("The rejection ratio 'Tr' is perform thanks to the formula", strong("(4.)")),
          h5(withMathJax("$$Tr = \\frac{(n~times~p.value < 0.05)* 100}{n}$$")),
          tags$br(),
          hr(),
          h4(strong("Spatial exploration")),
          tags$br(),
          p(style = "text-align: justify;", "In this panel, you can perform a spatialised analysis of your data starting from a Benn diagram or from projections.
          In both cases, you need to select the data you want to explor more locally in a new analysis.
          From the projections, you can select the points you want to explore directly.
          In the Benn diagram, you need to select the lasso selector.
          Pay attention to the number of points selected and check that the new sample contains enough points to carry out the new analysis.
          If this is not the case, you can lower the slider in the left sidebar panel to perform the analysis with fewer neighbours.
          You can dowload the new samples you have selected from the corresponding subpanels.
          "),
          tags$br(),
          hr(),
          h4(strong("Markdown report")),
          tags$br(),
          p("This panel is in progress. It will be available in the next version. You will find only a beta test."),
          tags$br(),
          hr(),
        )
      )
    ),
    tabPanel(
      "New models",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka4.png", height = 200),
          tags$br()
        ),
        column(12,
          align = "justify",
          h2("New models"),
          hr(),
          h5("Work in progress. This panel will be available in the next version."),
          hr(),
          tags$br()
        )
      )
    ),
    tabPanel(
      "Bibliography",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka4.png", height = 200),
          tags$br()
        ),
        column(12,
          align = "justify",
          h2("Bibliography"),
          hr(),
          h5("Benn D.I., 1994 - Fabric shape and the interpretation of sedimentary fabric data. Journal of Sedimentary Research, A64(4), pp. 910-915."),
          h5("Bertran P., Lenoble A. 2006 - Fabriques des niveaux archéologiques : Méthode et premier bilan des apports à l'étude taphonomique des sites paléolithiques. Paléo 14, pp. 13-28."),
          h5("Curray J.R., 1956 - Analysis of two-dimensional orientation data. Journal of Geology, 64, pp. 117-134."),
          h5("Krumbein W. C., 1939 - Preferred orientation of pebbles in sedimentary deposits, The journal of Geology, v. XLVII, n°7, pp. 673-706"),
          h5("McPherron S.P., 2018 - Additional statistical and graphical methods for analyzing site formation processes using artifact orientations, PloS One 13, 21p."),
          h5("Thomas, M., Discamps, E., Gravina, B., Texier, J.-P. (2019). Analyse taphonomique et spatiale de palimpsestes d’occupations moustériennes de l’abri inférieur du Moustier (Dordogne, France). Paléo 30(1), p. 278-299. https://doi.org/10.4000/paleo.4897"),
          h5("Texier, J.-P., Discamps, E., Gravina, B., Thomas, M. (2020). Les dépôts de remplissage de l’abri inférieur du Moustier (Dordogne, France) : lithostratigraphie, processus de formation et évolution du système géomorphologique. Paléo 30(2), p. 320-345. https://doi.org/10.4000/paleo.5826"),
          h5("Woodcock N.H., 1977 - Specification of fabric shapes using an eigenvalue method. Geological Society of America Bulletin, 88, pp. 1231-1236."),
          hr(),
          tags$br()
        )
      )
    )
  )
)


### --- server ---

server <- function(input, output, session) {
  # increase file upload size limit in shiny to 50MB
  options(shiny.maxRequestSize = 350 * 1024^2)

  # get the user data
  user_data <- reactive({
    req(!is.null(input$user_data))
    inFile <- input$user_data
    if (input$decimal == ".") {
      data <- read.csv(inFile$datapath, sep = input$sep)
    } else {
      data <- read.csv2(inFile$datapath, sep = input$sep)
    }
    return(data)
  })

  output$user_data_head <- renderTable(
    head(user_data(), 1)
  )

  output$id1 <- renderUI({
    selectInput("id", "unique 'ID'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$code1 <- renderUI({
    selectInput("code", "'Code' (e.g. silex, fauna)",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$sample1 <- renderUI({
    selectInput("sample", "'Sample' (e.g. US, UL)",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$x1 <- renderUI({
    selectInput("x", "'X'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$y1 <- renderUI({
    selectInput("y", "'Y'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$z1 <- renderUI({
    selectInput("z", "'Z'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$x2 <- renderUI({
    selectInput("x2", "'X2'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$y2 <- renderUI({
    selectInput("y2", "'Y2'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$z2 <- renderUI({
    selectInput("z2", "'Z2'",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$direction1 <- renderUI({
    selectInput("direction", "'Direction' (i.e. 0° to 360° angles)",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  output$dip1 <- renderUI({
    selectInput("dip", "'Plunge' (i.e. 0° to 90° angles)",
      choices = c("NULL", names(user_data())),
      selected = "NULL"
    )
  })

  clean_user_data <- reactive({

    req(input$user_data)

    dataaa <- user_data()
    DataNew1 <- dataaa

    id <- as.character(input$id)
    code <- as.factor(input$code)
    sample <- as.factor(input$sample)
    x <- input$x
    y <- input$y
    z <- input$z
    direction <- as.numeric(input$direction)
    dip <- as.numeric(input$dip)
    x2 <- input$x2
    y2 <- input$y2
    z2 <- input$z2

    colnames(DataNew1)[which(colnames(DataNew1) == id)] <- "id"
    colnames(DataNew1)[which(colnames(DataNew1) == code)] <- "code"
    colnames(DataNew1)[which(colnames(DataNew1) == sample)] <- "sample"
    colnames(DataNew1)[which(colnames(DataNew1) == x)] <- "X1"
    colnames(DataNew1)[which(colnames(DataNew1) == y)] <- "Y1"
    colnames(DataNew1)[which(colnames(DataNew1) == z)] <- "Z1"
    colnames(DataNew1)[which(colnames(DataNew1) == direction)] <- "direction"
    colnames(DataNew1)[which(colnames(DataNew1) == dip)] <- "dip"
    colnames(DataNew1)[which(colnames(DataNew1) == x2)] <- "X2"
    colnames(DataNew1)[which(colnames(DataNew1) == y2)] <- "Y2"
    colnames(DataNew1)[which(colnames(DataNew1) == z2)] <- "Z2"

    return(DataNew1)
  })


  ######### Modifier colonne sample à l'import, il faut que ce soit des characters sinon filter ne fonctionne pas
  # format the data to include all columns needed for the app' in a reactive function
  app_data <- reactive({

    if(is.null(input$user_data)){
      #req(input$user_data)
      #req(input$data_type)
      
      two_shots_example()
      
    } else {

    if (input$data_type == "Case 1: Only angles") {
      clean_user_data() %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            direction <= 180 ~ round(direction, 0),
            direction > 180 ~ round(direction - 180, 0)
          )
        ) %>%
        dplyr::mutate(
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = 0) %>%
        dplyr::mutate(Y1 = 0) %>%
        dplyr::mutate(Z1 = 0) %>%
        dplyr::rename(bearing = direction) %>%
        dplyr::rename(plunge = dip) %>%
        dplyr::mutate(X2 = round(X1 + cos(rad(plunge)) * sin(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Y2 = round(Y1 + cos(rad(plunge)) * cos(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Z2 = round(Z1 - sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::mutate(across(where(is.numeric), round, 3)) %>%
        dplyr::mutate(across(where(is.numeric), round, 3))
    } else if (input$data_type == "Case 2: Only angles from DistoX2") {
      clean_user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & direction < 180) ~ round(direction + 180, 0),
            (dip > 0 & direction > 180) ~ round(direction - 180, 0),
            dip <= 0 ~ round(direction, 0)
          ),
          plunge = abs(round(dip, 0)),
          orientation_pi = dplyr::case_when(
            direction <= 180 ~ round(direction, 0),
            direction > 180 ~ round(direction - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = 0) %>%
        dplyr::mutate(Y1 = 0) %>%
        dplyr::mutate(Z1 = 0) %>%
        # dplyr::rename(bearing = direction) %>%
        # dplyr::rename(plunge = dip) %>%
        dplyr::mutate(X2 = round(X1 + cos(rad(plunge)) * sin(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Y2 = round(Y1 + cos(rad(plunge)) * cos(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Z2 = round(Z1 - sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::select(-c(direction, dip)) %>%
        dplyr::mutate(across(where(is.numeric), round, 3))
    } else if (input$data_type == "Case 3: Angles from DistoX2 with coordinates") {
      clean_user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & direction < 180) ~ round(as.numeric(direction) + 180, 0),
            (dip > 0 & direction > 180) ~ round(as.numeric(direction) - 180, 0),
            dip <= 0 ~ round(as.numeric(direction), 0)
          ),
          plunge = abs(round(as.numeric(dip), 0)),
          orientation_pi = dplyr::case_when(
            direction <= 180 ~ round(direction, 0),
            direction > 180 ~ round(direction - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = as.numeric(x)) %>%
        dplyr::mutate(Y1 = as.numeric(y)) %>%
        dplyr::mutate(Z1 = as.numeric(z)) %>%
        dplyr::mutate(X2 = round(X1 + cos(rad(plunge)) * cos(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Y2 = round(Y1 + cos(rad(plunge)) * sin(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Z2 = round(Z1 - sin(rad(plunge)) * 0.05, 3)) %>%
        dplyr::select(-c(x, y, z, direction, dip)) %>%
        dplyr::mutate(across(where(is.numeric), round, 3))
    } else if (input$data_type == "Case 5: Two shots data without angles") {
      clean_user_data() %>%
        dplyr::mutate(plunge = round(plunge_and_bearing(clean_user_data())[[1]], 2)) %>%
        dplyr::mutate(bearing = round(plunge_and_bearing(clean_user_data())[[2]], 2)) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing <= 180 ~ bearing,
            bearing > 180 ~ bearing - 180
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(across(where(is.numeric), round, 3))
    } else { # Case 4: Angles and coordinates
      clean_user_data() %>%
        dplyr::rename(bearing = direction) %>%
        dplyr::rename(plunge = dip) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing <= 180 ~ bearing,
            bearing > 180 ~ bearing - 180
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = x) %>%
        dplyr::mutate(Y1 = y) %>%
        dplyr::mutate(Z1 = z) %>%
        dplyr::mutate(X2 = round(x + cos(rad(plunge)) * sin(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Y2 = round(y + cos(rad(plunge)) * cos(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Z2 = round(z - sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::select(-c(x, y, z)) %>%
        dplyr::mutate(across(where(is.numeric), round, 3))
      }
    }
  })

  # The user sees the data included in the reactive function app_data() and can download it
  output$app_data_view <- DT::renderDataTable(
    app_data(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(app_data()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )


  # Creation of the data I will use in the examples files - disto_one_shot
  distoX2_xyz_example <- reactive({
    distoX2 <-
      data.frame(
        id = c(7996, 7987, 8056, 8052, 8010, 7674, 7672, 7660, 7984, 7669, 7647, 7678, 7904, 476, 514, 455, 573, 681, 20075, 976, 20112, 20091, 20100, 20062, 893, 20059, 20079, 20253, 20164, 20348, 20346, 20176, 20330, 20276, 20203, 20281, 20280, 20616, 20600, 20652, 20665, 20377, 20542, 20457, 20608, 20416, 20619, 20595, 20384, 20465, 20601, 20651, 20455, 20459, 20434, 20675, 20471, 20436, 20797, 20965, 21069, 20828, 21077, 20929, 20994, 20930, 20860, 20950, 20964, 20799, 20781, 20838, 20995, 20833, 20846, 20966, 21247, 21079, 21179, 21165, 20782, 21169, 20809, 21230, 20980, 21076, 20785, 21194, 20803, 20819, 21515, 21368, 21371, 21341, 21488, 21294, 21582, 21602, 21560, 21532, 21458, 21310, 21350, 21351, 21358, 21283, 21430, 21362, 21394, 21544, 21642, 21364, 21645, 21573, 21563, 21578, 21385, 21569, 21552, 21865, 21809, 21841, 21808, 22013, 21895, 21930, 22103, 22150, 22154, 22042, 22076, 21918, 21883, 22082, 22033, 21974, 21928, 22086, 22094, 22140, 22697, 22579, 22572, 22629, 22635, 22296, 22302, 22341, 22701, 22473, 22168, 22463, 22582, 22656, 22717, 22234, 22616, 22256, 22609),
        x = c(98.562, 98.725, 98.63, 98.496, 98.479, 98.861, 98.806, 98.644, 98.594, 98.76, 98.848, 98.84, 98.59, 98.669, 98.514, 98.763, 98.567, 98.823, 98.724, 98.675, 98.652, 98.656, 98.581, 98.546, 98.678, 98.594, 98.555, 98.632, 98.663, 98.6, 98.675, 98.633, 98.647, 98.734, 98.705, 98.552, 98.551, 98.662, 98.659, 98.581, 98.588, 98.761, 98.756, 98.784, 98.649, 98.81, 98.652, 98.696, 98.806, 98.734, 98.666, 98.595, 98.749, 98.772, 98.782, 98.613, 98.731, 98.78, 98.847, 98.66, 98.604, 98.797, 98.633, 98.699, 98.606, 98.688, 98.731, 98.673, 98.658, 98.823, 98.812, 98.781, 98.629, 98.768, 98.798, 98.706, 98.589, 98.618, 98.566, 98.578, 98.831, 98.627, 98.804, 98.557, 98.655, 98.625, 98.789, 98.549, 98.799, 98.77, 98.719, 98.843, 98.848, 98.775, 98.764, 98.755, 98.667, 98.714, 98.773, 98.718, 98.769, 98.851, 98.867, 98.87, 98.814, 98.849, 98.769, 98.807, 98.789, 98.728, 98.614, 98.854, 98.615, 98.698, 98.693, 98.664, 98.844, 98.709, 98.694, 98.508, 98.63, 98.833, 98.844, 98.586, 98.744, 98.631, 98.725, 98.55, 98.53, 98.755, 98.742, 98.681, 98.784, 98.548, 98.837, 98.564, 98.614, 98.641, 98.745, 98.642, 98.68, 98.5763, 98.6778, 98.5254, 98.6229, 98.8599, 98.7227, 98.8526, 98.6379, 98.6254, 98.791, 98.792, 98.7345, 98.5572, 98.5793, 98.79, 98.5352, 98.702, 98.6133),
        y = c(47.65, 47.767, 47.273, 47.876, 47.457, 47.709, 47.317, 47.572, 47.894, 47.465, 47.78, 47.671, 47.485, 47.578, 47.755, 47.603, 47.604, 47.372, 47.776, 47.565, 47.899, 47.87, 47.593, 47.787, 47.329, 47.959, 47.735, 47.752, 47.335, 47.294, 47.333, 47.926, 47.41, 47.599, 47.945, 47.564, 47.574, 47.347, 47.317, 47.961, 47.377, 47.795, 47.925, 47.425, 47.78, 47.284, 47.829, 47.672, 47.38, 47.703, 47.946, 47.955, 47.52, 47.35, 47.497, 47.801, 47.344, 47.44, 47.431, 47.8, 47.453, 47.563, 47.702, 47.272, 47.588, 47.456, 47.907, 47.344, 47.877, 47.651, 47.332, 47.708, 47.823, 47.631, 47.604, 47.917, 47.304, 47.323, 47.754, 47.726, 47.527, 47.889, 47.545, 47.909, 47.513, 47.701, 47.288, 47.818, 47.325, 47.531, 47.348, 47.655, 47.493, 47.959, 47.492, 47.298, 47.372, 47.407, 47.258, 47.585, 47.613, 47.435, 47.723, 47.686, 47.517, 47.595, 47.72, 47.409, 47.474, 47.799, 47.344, 47.475, 47.945, 47.577, 47.878, 47.476, 47.605, 47.66, 47.475, 47.329, 47.479, 47.733, 47.461, 47.473, 47.392, 47.78, 47.662, 47.316, 47.644, 47.548, 47.636, 47.583, 47.584, 47.656, 47.312, 47.765, 47.737, 47.76, 47.447, 47.404, 47.326, 47.3899, 47.2996, 47.3856, 47.5898, 47.632, 47.5866, 47.4178, 47.4118, 47.6306, 47.913, 47.5581, 47.4206, 47.565, 47.392, 47.904, 47.5913, 47.777, 47.5959),
        z = c(-1.143, -1.16, -1.137, -1.126, -1.13, -1.17, -1.145, -1.141, -1.159, -1.163, -1.15, -1.156, -1.14, -1.175, -1.18, -1.188, -1.192, -1.215, -1.253, -1.256, -1.262, -1.265, -1.256, -1.261, -1.261, -1.253, -1.256, -1.271, -1.271, -1.276, -1.279, -1.263, -1.283, -1.272, -1.27, -1.273, -1.277, -1.293, -1.299, -1.283, -1.292, -1.278, -1.303, -1.289, -1.283, -1.29, -1.289, -1.306, -1.282, -1.294, -1.294, -1.289, -1.282, -1.287, -1.28, -1.287, -1.274, -1.284, -1.317, -1.326, -1.308, -1.321, -1.342, -1.304, -1.312, -1.329, -1.33, -1.329, -1.32, -1.328, -1.307, -1.321, -1.333, -1.312, -1.325, -1.341, -1.33, -1.332, -1.329, -1.335, -1.31, -1.347, -1.316, -1.345, -1.328, -1.337, -1.312, -1.335, -1.326, -1.308, -1.338, -1.354, -1.357, -1.372, -1.361, -1.331, -1.353, -1.36, -1.345, -1.349, -1.344, -1.34, -1.356, -1.353, -1.351, -1.344, -1.361, -1.331, -1.353, -1.363, -1.358, -1.351, -1.369, -1.35, -1.354, -1.346, -1.359, -1.35, -1.343, -1.35, -1.371, -1.37, -1.371, -1.375, -1.372, -1.373, -1.381, -1.379, -1.371, -1.378, -1.389, -1.361, -1.369, -1.383, -1.373, -1.38, -1.371, -1.383, -1.377, -1.374, -1.3986, -1.3821, -1.3925, -1.3741, -1.408, -1.4016, -1.3877, -1.4061, -1.4066, -1.3977, -1.393, -1.4028, -1.4023, -1.3897, -1.3822, -1.401, -1.3904, -1.402, -1.3966),
        code = c("FAUNE", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "FAUNE", "FAUNE", "FAUNE", "FAUNE", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "FAUNE", "FAUNE", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "FAUNE", "SILEX", "FAUNE", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "SILEX", "FAUNE", "SILEX", "SILEX", "FAUNE", "SILEX"),
        sample = c("AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII", "AIII"),
        direction = c(10.26, 35.73, 58.08, 101.2, 132.78, 140.55, 206.6, 209.61, 231, 290.96, 308.13, 322.83, 332.53, 191.37, 221.08, 325.65, 223.18, 209.17, 12.52, 70.19, 105.5, 149.77, 167.19, 196.72, 209.01, 273.19, 276.37, 41.14, 77.78, 91.63, 92.7, 101.73, 106.28, 182.02, 288.39, 325.09, 330.58, 2.74, 16.76, 121.04, 161.99, 174.51, 176.61, 191.02, 192.75, 196.81, 196.97, 199.12, 213.43, 252.75, 273.18, 300.17, 308.34, 323.01, 327.89, 337.27, 338.35, 355.13, 34, 63.86, 83.8, 93.03, 144.51, 157.23, 178.87, 190.81, 190.83, 200.23, 202.71, 210.28, 211.55, 228.14, 231.16, 231.61, 248.33, 251.04, 256.61, 270.57, 272.84, 273.63, 283.28, 295.55, 296.47, 304.32, 308.66, 335.23, 342.53, 343.17, 344, 351.78, 3.91, 14.12, 25.91, 32.67, 102.11, 104.33, 151.31, 170.44, 182.37, 190.01, 192.5, 192.89, 199.48, 201.35, 203.53, 206.77, 220.35, 229.68, 235.55, 256.78, 266.2, 267.19, 277.32, 286.98, 290.82, 299.95, 301.82, 314.21, 345.1, 9.77, 275.64, 279.01, 312.36, 1, 4.74, 13.87, 14.45, 24.72, 29.92, 40.79, 281.44, 284.84, 299.3, 318.88, 320.04, 323.51, 327.83, 351.16, 353.3, 355.12, 4.17, 11.59, 15.96, 18.59, 27.25, 44.73, 45.3, 51.59, 62.8, 88.01, 126.9, 230.87, 294.43, 298.73, 309.31, 330.66, 338.78, 343.19, 355.12),
        dip = c(5, 1.24, 5.76, 3.54, 3.65, 10.48, 3.54, 2.22, 0.51, 2.18, 4.19, 1.17, 0.74, 12.35, 35.75, 8.49, 15.5, 9.42, 22.45, 43.05, 6, 16.75, 12.62, 3.76, 12.38, 3.88, 19.24, 0.71, 15.81, 20.28, 13.91, 13.52, 15.86, 14.81, 1.65, 7.34, 6.81, 14.39, 3.26, 0.85, 1.09, 2.75, 2.19, 11.29, 18.51, 0.22, 7.03, 6.59, 15.43, 17.8, 30.3, 5.42, 8.46, 16.85, 29.4, 10.39, 10.47, 6.47, 0.87, 17.72, 36.46, 0.25, 0.19, 19.61, 3.42, 17.5, 3.74, 10.42, 12.89, 3.27, 11.16, 14.42, 20.02, 6.41, 5.19, 5.51, 8.74, 7.03, 27.67, 22.2, 10.76, 11.27, 28.26, 29.75, 19.69, 0.65, 0.31, 3.55, 5.15, 3.53, 0.12, 8.39, 10.51, 20.51, 17.02, 1.05, 12.09, 9.46, 0.75, 2.3, 1.75, 17.02, 1.97, 11.14, 1.8, 18.81, 20.35, 13.47, 14.69, 8.13, 1.96, 7.34, 16.31, 4.66, 18, 0.85, 24.26, 16.17, 7.76, 1.99, 8.88, -6.57, -1.14, -0.12, 1.23, 8.09, -3.15, 23.45, 34.14, -7.47, 20.67, 1.92, 3.73, -12.41, 3.63, -5.11, 1.04, -11.48, -4.24, 5.84, 3.36, -8.1, 12.12, -21.14, 4.98, 5.22, 0.26, -4.99, 1.05, -0.48, -15.7, -8.64, 7.32, 1.19, -0.71, -2.11, -12.84, -11.43, 0.1)
      )
    return(distoX2)
  })


  # if disto angles only
  distoX2_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::select(id, code, sample, direction, dip)
  })


  # if angles only
  angles_only_example <- reactive({
    data_to_modif <- distoX2_xyz_example()

    angles_data <- data_to_modif %>%
      dplyr::select(-c(x, y, z)) %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & direction < 180) ~ round(as.numeric(direction) + 180, 0),
          (dip > 0 & direction > 180) ~ round(as.numeric(direction) - 180, 0),
          dip <= 0 ~ round(as.numeric(direction), 0)
        )
      ) %>%
      dplyr::mutate(plunge = abs(round(as.numeric(dip), 0))) %>%
      dplyr::select(-c(direction, dip)) %>%
      dplyr::rename(direction = bearing) %>%
      dplyr::rename(dip = plunge)

    return(angles_data)
  })


  # if angles and coordinates
  angles_xyz_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & direction < 180) ~ round(as.numeric(direction) + 180, 0),
          (dip > 0 & direction > 180) ~ round(as.numeric(direction) - 180, 0),
          dip <= 0 ~ round(as.numeric(direction), 0)
        ),
        plunge = abs(round(as.numeric(dip), 0))
      ) %>%
      dplyr::select(-c(direction, dip)) %>%
      dplyr::rename(direction = bearing) %>%
      dplyr::rename(dip = plunge)
  })


  # if two shots data
  two_shots_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & direction < 180) ~ round(as.numeric(direction) + 180, 0),
          (dip > 0 & direction > 180) ~ round(as.numeric(direction) - 180, 0),
          dip <= 0 ~ round(as.numeric(direction), 0)
        ),
        plunge = abs(round(as.numeric(dip), 0))
      ) %>%
      dplyr::mutate(X1 = as.numeric(x)) %>%
      dplyr::mutate(Y1 = as.numeric(y)) %>%
      dplyr::mutate(Z1 = as.numeric(z)) %>%
      dplyr::mutate(X2 = round(X1 + cos(rad(plunge)) * sin(rad(bearing)) * 0.05, 3)) %>%
      dplyr::mutate(Y2 = round(Y1 + cos(rad(plunge)) * cos(rad(bearing)) * 0.05, 3)) %>%
      dplyr::mutate(Z2 = round(Z1 - sin(rad(plunge)) * 0.05, 3)) %>%
      dplyr::select(-c(x, y, z, direction, dip, bearing, plunge)) %>%
      dplyr::mutate(across(where(is.numeric), round, 3))
  })


  # function to download the file if angle only
  output$angles_only <- downloadHandler(
    filename = function() {
      paste("Case 1: Only angles", ".csv")
    },
    content = function(file) {
      write.csv2(angles_only_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # function to download the file if angle only from disto
  output$angles_only_disto <- downloadHandler(
    filename = function() {
      paste("Case 2: Only angles from DistoX2", ".csv")
    },
    content = function(file) {
      write.csv2(distoX2_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # function to download the file if angle from disto and xyz
  output$disto_one_shot <- downloadHandler(
    filename = function() {
      paste("Case 3: Angles from DistoX2 with coordinates", ".csv")
    },
    content = function(file) {
      write.csv2(distoX2_xyz_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # function to download the file if angle and xyz
  output$angles_one_shot <- downloadHandler(
    filename = function() {
      paste("Case 4: Angles and coordinates", ".csv")
    },
    content = function(file) {
      write.csv2(angles_xyz_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )


  # function to download the file if two shots data
  output$two_shots <- downloadHandler(
    filename = function() {
      paste("Case 5: Two shots data without angles", ".csv")
    },
    content = function(file) {
      write.csv2(two_shots_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  ### --- Classical method ---

  # User chooses levels to display
  output$sample_c <- renderUI({
    req(input$user_data)
    radioButtons("sample_c", label = "Filter by sample", choices = unique(as.character(app_data()$sample)))
  })


  # User chooses codes to display
  output$code_c <- renderUI({
    req(input$user_data)
    if (input$plot_all_samples_c) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_c)
    }
    radioButtons("code_c", label = "Filter by code", choices = unique(app_data_temp$code))
  })


  # User chooses the model to display
  model_subset <- reactive({
    ternary_model <- ternary_model %>%
      dplyr::filter(Type %in% input$model)
  })


  # get the data selected by the user (filter by sample and code)
  subset_code_sample_c <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$sample_c)
    if (input$plot_all_samples_c & input$plot_all_codes_c) {
      data_sub <- app_data()
    } else if (input$plot_all_samples_c & !input$plot_all_codes_c) {
      data_sub <- subset(app_data(), code == input$code_c)
    } else if (!input$plot_all_samples_c & input$plot_all_codes_c) {
      data_sub <- subset(app_data(), sample == input$sample_c)
    } else {
      data_sub <- subset(app_data(), code == input$code_c & sample == input$sample_c)
    }
    return(data_sub)
  })


  # calculating benn indices for the user subset data
  benn_index_c <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$minimum_n) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )

      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)

        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))

          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])

          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }

      return(benn)
    }


    benn_ind <- data.frame(benn(subset_code_sample_c(), level = subset_code_sample_c()$sample)) %>%
      dplyr::mutate(PL = 1 - IS - EL)
    benn_ind$Sample <- rownames(benn_ind)
    return(benn_ind)
  })

  # benn_plot function
  benn_plot <- reactive({
    tern_model <- model_subset()

    p <- ggtern(tern_model, aes(as.numeric(PL), as.numeric(IS), as.numeric(EL))) +
      geom_polygon(aes(fill = Type, group = area, alpha = 0.1)) +
      scale_fill_jco() +
      ggplot2::labs(title = "", fill = "Process", x = "PL", y = "IS", z = "EL") +
      ggplot2::theme(
        tern.axis.line.T = element_line(color = "black", size = 1),
        tern.axis.line.L = element_line(color = "black", size = 1),
        tern.axis.line.R = element_line(color = "black", size = 1)
      ) +
      ggtern::theme_bw() +
      ggplot2::guides(alpha = "none", size = 0.5) +
      ggtern::scale_T_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_L_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_R_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::geom_Tline(Tintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Lline(Lintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Rline(Rintercept = 0.5, colour = "black", size = 0.2)

    r <- p +
      ggplot2::geom_point(data = benn_index_c(), aes(PL, IS, EL, color = Sample), size = 2.1) +
      ggplot2::scale_color_manual(values = c(
        "#0073C2FF",
        "#EFC000FF",
        "#868686FF",
        "#526E2DFF",
        "#935931FF",
        "#E89242FF",
        "#CD534CFF",
        "#7AA6DCFF",
        "#003C67FF",
        "#8F7700FF",
        "#A73030FF",
        "#4A6990FF",
        "#FAE48BFF",
        "#E887CCFF"
      )) #+
    # ggplot2::scale_color_brewer(palette = "Paired")

    return(r)
  })

  # print the benn plot
  output$benn <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(benn_plot())
  })

  # download the benn plot
  output$download_benn_c <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_c == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_c == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      print(benn_plot())
      dev.off()
    }
  )

  # orientation rose diagram
  orientation_rose_c <- reactive({
    #req(input$user_data)
    req(input$data_type)
    d <- subset_code_sample_c()$orientation_pi + 180
    e <- c(subset_code_sample_c()$orientation_pi, d)
    f <- circular(e,
      type = "angles", units = "degrees",
      template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })

  # plot orientation_rose_c
  output$orientation_rose_c <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(orientation_rose_c())
  })

  # plunge rose diagram
  plunge_rose_c <- reactive({
    #req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(subset_code_sample_c()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })

  # plot plunge_rose_c
  output$plunge_rose_c <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(plunge_rose_c())
  })

  # download rose diagram
  output$download_orientation_rose_c <- downloadHandler(
    filename = function() {
      paste("orientation_rose", input$file_format_rose_orientation, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_orientation == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_orientation == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(orientation_rose_c())
      dev.off()
    }
  )

  # download rose plunge
  output$download_plunge_rose_c <- downloadHandler(
    filename = function() {
      paste("plunge_rose", input$file_format_rose_plunge, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_plunge == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_plunge == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(plunge_rose_c())
      dev.off()
    }
  )

  # schmidt diagram
  schmidt_diagram_c <- reactive({
    #req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(subset_code_sample_c()$bearing,
      subset_code_sample_c()$plunge,
      pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })

  # plot schmidt_diagram_c
  output$schmidt_diagram_c <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(schmidt_diagram_c())
  })

  output$download_schmidt_c <- downloadHandler(
    filename = function() {
      paste("schmidt_diagram", input$file_format_schmidt_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_c == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_c == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(schmidt_diagram_c())
      dev.off()
    }
  )

  # data for summary table
  summary_data <- reactive({
    level <- unique(subset_code_sample_c()$sample)
    results <- data.frame(matrix(nrow = length(level), ncol = 5))
    for (i in level) {
      data_level <- subset(subset_code_sample_c(), sample == i)
      if (nrow(data_level >= input$minimum_n)) {
        R1 <- circular::rayleigh.test(2 * circular::circular(data_level$orientation_pi,
          type = "angles",
          units = "degrees",
          modulo = "pi"
        ))
        R2 <- circular::rayleigh.test(2 * circular::circular(data_level$angle_double,
          type = "angles",
          units = "degrees",
          modulo = "pi"
        ))
        L <- round(R1$statistic * 100, 2)
        p <- round(R1$p.value, 2)
        L.double <- round(R2$statistic * 100, 2)
        p.double <- round(R2$p.value, 2)
        # SphVar <- varSph(data_level$bearing, data_level$plunge)
        results[i, ] <- c(i, L, p, L.double, p.double)
        column_names <- c("sample", "L", "R.p", "L.double", "R.p.double")
        colnames(results) <- column_names
      }
    }

    benn_indices <- benn_index_c() %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::rename(sample = Sample)
    table <- merge(x = benn_indices, y = results, by = "sample")
    return(table)
  })

  # Summary data table with all indices and orientation tests
  output$summary_table_c <- DT::renderDataTable(
    summary_data(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_data()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )

  ### -- spatialised method --

  # User chooses levels to display
  output$sample_sm <- renderUI({
    #req(input$user_data)
    radioButtons("sample_sm", label = "Filter by sample", choices = unique(app_data()$sample))
  })

  # User chooses codes to display
  output$code_sm <- renderUI({
    #req(input$user_data)
    # if(input$plot_all_samples_sm){
    #   app_data_temp <- app_data()
    # } else {
    app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_sm)
    # }
    radioButtons("code_sm", label = "Filter by code", choices = unique(app_data_temp$code))
  })

  # get the data selected by the user (filter by sample and code)
  subset_code_sample_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$sample_sm)
    # if (input$plot_all_samples_sm & input$plot_all_codes_sm) {
    #   data_sub <- app_data()
    # } else if (input$plot_all_samples_sm & !input$plot_all_codes_sm) {
    #   data_sub <- subset(app_data(), code == input$code_sm)
    # } else if (!input$plot_all_samples_sm & input$plot_all_codes_sm) {
    #   data_sub <- subset(app_data(), sample == input$sample_sm)
    # } else {
    #   data_sub <- subset(app_data(), code == input$code_sm & sample == input$sample_sm)
    # }

    if (input$plot_all_codes_sm) {
      data_sub <- subset(app_data(), sample == input$sample_sm)
    } else {
      data_sub <- subset(app_data(), sample == input$sample_sm & code == input$code_sm)
    }

    return(data_sub)
  })

  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))

  # calculating benn indices for the user subset data
  benn_index_sm <- reactive({
    req(input$n_nearest_sm)

    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_sm) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )

      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)

        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }

      return(benn)
    }


    spatial_benn <- function(xyz, nearest = input$n_nearest_sm, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )

      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }

      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]

      return(list(benn))
    }


    benn_sm <- data.frame(spatial_benn(subset_code_sample_sm())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)

    return(benn_sm)
  })

  # make a function to plot spatial benn ternary
  benn_sm_plot <- reactive({
    #req(input$user_data)
    req(input$data_type)
    data_to_plot <- benn_index_sm()

    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    recordPlot()
  })

  output$benn_sm <- renderPlot({
    benn_sm_plot()
  })

  ortho_xy_sm <- reactive({
    ortho <- raster::stack(input$ortho_xy_sm$datapath)
    return(ortho)
  })

  ortho_xz_sm <- reactive({
    ortho <- raster::stack(input$ortho_xz_sm$datapath)
    return(ortho)
  })

  ortho_yz_sm <- reactive({
    ortho <- raster::stack(input$ortho_yz_sm$datapath)
    return(ortho)
  })

  # XY spatial projection with points
  projection_points_xy_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xy_sm()@extent@xmin,
          ortho_xy_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_sm()@extent@ymin,
          ortho_xy_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with points
  projection_points_xz_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xz_sm()@extent@xmin,
          ortho_xz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_sm()@extent@ymin,
          ortho_xz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xz)
  })

  # YZ spatial projection with points
  projection_points_yz_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_yz_sm()@extent@xmin,
          ortho_yz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_sm()@extent@ymin,
          ortho_yz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(yz)
  })

  # XY spatial projection with sticks
  projection_sticks_xy_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Y1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Y1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xy_sm()@extent@xmin,
          ortho_xy_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_sm()@extent@ymin,
          ortho_xy_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with sticks
  projection_sticks_xz_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xz_sm()@extent@xmin,
          ortho_xz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_sm()@extent@ymin,
          ortho_xz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(xz)
  })

  # YZ spatial projection with sticks
  projection_sticks_yz_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$Y1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$Y2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$Y1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$Y2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_yz_sm()@extent@xmin,
          ortho_yz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_sm()@extent@ymin,
          ortho_yz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(yz)
  })

  # xy plot with colored points or sticks
  output$projection_xy_sm <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(req(projection_points_xy_sm()))
      } else {
        print(req(projection_sticks_xy_sm()))
      }
    },
    height = 500,
    width = 700
  )

  # xz plot with colored points or sticks
  output$projection_xz_sm <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(projection_points_xz_sm())
      } else {
        print(projection_sticks_xz_sm())
      }
    },
    height = 300,
    width = 780
  )

  # yz plot with colored points or sticks
  output$projection_yz_sm <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(projection_points_yz_sm())
      } else {
        print(projection_sticks_yz_sm())
      }
    },
    height = 300,
    width = 780
  )

  # dowload benn diagram
  output$download_benn_sm <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(benn_sm_plot())
      dev.off()
    }
  )

  # download xy projection
  output$download_xy_sm <- downloadHandler(
    filename = function() {
      paste("XY projection", input$file_format_xy_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xy_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xy_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_xy_sm())
        dev.off()
      } else {
        print(projection_sticks_xy_sm())
        dev.off()
      }
    }
  )

  # download xz projection
  output$download_xz_sm <- downloadHandler(
    filename = function() {
      paste("XZ projection", input$file_format_xz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xz_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xz_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_xz_sm())
        dev.off()
      } else {
        print(projection_sticks_xz_sm())
        dev.off()
      }
    }
  )

  # download xz projection
  output$download_yz_sm <- downloadHandler(
    filename = function() {
      paste("YZ projection", input$file_format_yz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_yz_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_yz_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_yz_sm())
        dev.off()
      } else {
        print(projection_sticks_yz_sm())
        dev.off()
      }
    }
  )

  # summary data at series scale
  summary_data_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    spatial_bearing <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }


    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }

    data <- subset_code_sample_sm()
    benn_indices <- round(benn_index_sm(), 2)
    rayleigh_test <- data.frame(spatial_bearing(data))
    rayleigh_double <- data.frame(spatial_bearing_double(data))

    summary_data <- cbind(data, benn_indices, rayleigh_test, rayleigh_double)
    return(summary_data)
  })

  # Summary data table with all indices and orientation tests for series
  output$summary_table_sm <- DT::renderDataTable(
    summary_data_sm(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_data_sm()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )



  # summary data at sample scale
  summary_data2_sm <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data <- summary_data_sm()
    level <- unique(summary_data_sm()$sample)
    results <- data.frame(matrix(nrow = length(level), ncol = 12))

    for (i in level) {
      data_level <- subset(data, sample == i)

      if (nrow(data_level >= input$minimum_n)) {
        N <- nrow(data_level)
        IS <- round(mean(data_level$IS), 2)
        EL <- round(mean(data_level$EL), 2)
        PL <- round(mean(data_level$PL), 2)
        sd.IS <- round(var(data_level$IS), 2)
        sd.EL <- round(var(data_level$EL), 2)
        sd.PL <- round(var(data_level$PL), 2)
        L <- round(mean(data_level$L), 2)
        Tr <- round(sum(data_level$R.p < 0.05) * 100 / nrow(data_level), 2)
        L.double <- round(mean(data_level$L.double), 2)
        Tr.double <- round(sum(data_level$R.p.double < 0.05) * 100 / nrow(data_level), 2)

        results[i, ] <- c(i, N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
        column_names <- c("sample", "N", "IS", "EL", "PL", "sd.IS", "sd.EL", "sd.PL", "L", "Tr", "L.double", "Tr.double")
        colnames(results) <- column_names
      }
    }
    level_data <- data.frame(sample = level)
    results_def <- merge(level_data, results, by = "sample")
    return(results_def)
  })

  # Summary data table2 with all indices and orientation tests at sample scale
  output$summary_table2_sm <- DT::renderDataTable(
    summary_data2_sm(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_data2_sm()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )

  ### --- spatial exploration ---

  # User chooses levels to display
  output$sample_se <- renderUI({
    #req(input$user_data)
    radioButtons("sample_se", label = "Filter by sample", choices = unique(app_data()$sample))
  })

  # User chooses codes to display
  output$code_se <- renderUI({
    #req(input$user_data)
    if (input$plot_all_samples_se) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_se)
    }
    radioButtons("code_se", label = "Filter by code", choices = unique(app_data_temp$code))
  })

  # get the data selected by the user (filter by sample and code)
  subset_code_sample_se <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$sample_se)
    if (input$plot_all_samples_se & input$plot_all_codes_se) {
      data_sub <- app_data()
    } else if (input$plot_all_samples_se & !input$plot_all_codes_se) {
      data_sub <- subset(app_data(), code == input$code_se)
    } else if (!input$plot_all_samples_se & input$plot_all_codes_se) {
      data_sub <- subset(app_data(), sample == input$sample_se)
    } else {
      data_sub <- subset(app_data(), code == input$code_se & sample == input$sample_se)
    }
    return(data_sub)
  })

  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))

  # calculating benn indices for the user subset data
  benn_index_se <- reactive({
    req(input$n_nearest_se)

    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }

    spatial_benn <- function(xyz, nearest = input$n_nearest_se, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      return(list(benn))
    }
    # }

    # spatial_bearing = function(xyz, nearest = input$n_nearest_se) {
    #
    #   # Make a place to hold the computed mean bearings of nearest neighbors
    #   near_avg_bearing_p = vector(mode = "numeric", length = nrow(xyz))
    #   near_avg_bearing_Rbar = vector(mode = "numeric", length = nrow(xyz))
    #
    #   # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
    #   for (k in 1:nrow(xyz)) {
    #     centerx = xyz$X1[k]
    #     centery = xyz$Y1[k]
    #     centerz = xyz$Z1[k]
    #     d = sqrt((centerx-xyz$X1)^2 + (centery-xyz$Y1)^2 + (centerz-xyz$Z1)^2)
    #     xyz_subsample = xyz[order(d)[1:nearest],]
    #
    #     # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
    #     near_avg_bearing_p[k] = round(r.test(2*xyz_subsample$orientation_pi, degree=TRUE)$p.value, 2)
    #     near_avg_bearing_Rbar[k] = round(r.test(2*xyz_subsample$orientation_pi, degree=TRUE)$r.bar, 2)
    #   }
    #
    #   # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
    #   return(list(L = near_avg_bearing_Rbar*100, R.p = near_avg_bearing_p)) }
    #
    #
    # spatial_bearing_double = function(xyz, nearest = input$n_nearest_se) {
    #
    #   # Make a place to hold the computed mean bearings of nearest neighbors
    #   near_avg_bearing_p = vector(mode = "numeric", length = nrow(xyz))
    #   near_avg_bearing_Rbar = vector(mode = "numeric", length = nrow(xyz))
    #
    #   # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
    #   for (k in 1:nrow(xyz)) {
    #     centerx = xyz$X1[k]
    #     centery = xyz$Y1[k]
    #     centerz = xyz$Z1[k]
    #     d = sqrt((centerx-xyz$X1)^2 + (centery-xyz$Y1)^2 + (centerz-xyz$Z1)^2)
    #     xyz_subsample = xyz[order(d)[1:nearest],]
    #
    #     # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
    #     near_avg_bearing_p[k] = round(r.test(2*xyz_subsample$angle_double,degree=TRUE)$p.value, 2)
    #     near_avg_bearing_Rbar[k] = round(r.test(2*xyz_subsample$angle_double,degree=TRUE)$r.bar, 2)
    #   }

    # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
    # return(list(L.double = near_avg_bearing_Rbar*100, R.p.double = near_avg_bearing_p)) }

    # rayleigh_test <- round(data.frame(spatial_bearing(subset_code_sample_se())), 2)
    # rayleigh_double <- round(data.frame(spatial_bearing_double(subset_code_sample_se())), 2)

    benn_se <- data.frame(spatial_benn(subset_code_sample_se())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)

    data <- cbind(subset_code_sample_se(), benn_se) # , rayleigh_test, rayleigh_double)
    return(data)
  })

  # interactive ternary diagram
  interactive_ternary_plot <- reactive({
    data_to_plot <- benn_index_se()

    colors <- data_to_plot %>%
      dplyr::select(IS, EL, PL) %>%
      dplyr::mutate(color_code = rgb(IS, EL, PL)) %>%
      dplyr::select(color_code)
    colors_p <- setNames(colors$color_code, data_to_plot$id)

    fig <- plot_ly(
      data = data_to_plot,
      a = ~IS, b = ~PL, c = ~EL,
      customdata = ~id,
      type = "scatterternary",
      mode = "markers",
      marker = list(
        size = 5,
        color = colors_p
      ),
      text = ~ paste("sample:", sample, "<br>code", code)
    ) %>%
      layout(
        title = "",
        ternary = list(
          aaxis = list(title = "IS"),
          baxis = list(title = "PL"),
          caxis = list(title = "EL")
        )
      )

    event_register(fig, "plotly_selected")

    return(fig)
  })

  # plot the interactive ternary
  output$interactive_ternary <- renderPlotly({
    interactive_ternary_plot()
  })

  # download XY ortho path
  ortho_xy_se <- reactive({
    ortho <- raster::stack(input$ortho_xy_se$datapath)
    return(ortho)
  })

  # download XZ ortho path
  ortho_xz_se <- reactive({
    ortho <- raster::stack(input$ortho_xz_se$datapath)
    return(ortho)
  })

  # download YY ortho path
  ortho_yz_se <- reactive({
    ortho <- raster::stack(input$ortho_yz_se$datapath)
    return(ortho)
  })

  # get in a reactive function the selected data
  selected_data <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }

    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )

      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }

      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]

      return(list(benn))
    }

    plotly_event_data <- event_data(event = "plotly_selected", priority = "event")
    req(plotly_event_data)

    selected <- filter(benn_index_se(), id %in% plotly_event_data$customdata) %>%
      dplyr::select(-c(EL, IS, PL))

    new_benn <- cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      mutate(PL = 1 - EL - IS)

    return(new_benn)
  })

  # plot the selected data in a new benn diagram
  selected_benn_se1 <- reactive({
    data_to_plot <- selected_data() %>%
      dplyr::select(EL, IS, PL)

    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    recordPlot()
  })

  output$selected_benn <- renderPlot({
    selected_benn_se1()
  })

  # download benn diagram
  output$download_benn_se1 <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(selected_benn_se1())
      dev.off()
    }
  )

  # orientation rose diagram for selected data
  orientation_rose_se <- reactive({
    #req(input$user_data)
    req(input$data_type)
    d <- selected_data()$orientation_pi + 180
    e <- c(selected_data()$orientation_pi, d)
    f <- circular(e,
      type = "angles", units = "degrees",
      template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })

  # plot orientation_rose_se for selected data
  output$orientation_rose_selected <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(orientation_rose_se())
  })

  # download orientation rose
  output$download_rose_o_se1 <- downloadHandler(
    filename = function() {
      paste("orientation_rose", input$file_format_rose_o_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_o_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_o_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(orientation_rose_se())
      dev.off()
    }
  )

  # plunge rose diagram for selected data
  plunge_rose_se <- reactive({
    #req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(selected_data()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })


  # plot plunge_rose_c for selected data
  output$plunge_rose_selected <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(plunge_rose_se())
  })


  # download plunge rose
  output$download_rose_p_se1 <- downloadHandler(
    filename = function() {
      paste("plunge_rose", input$file_format_rose_p_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_p_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_p_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(plunge_rose_se())
      dev.off()
    }
  )

  # schmidt diagram for selected data
  schmidt_diagram_se <- reactive({
    #req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(selected_data()$bearing,
      selected_data()$plunge,
      pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })


  # plot schmidt_diagram_c for selected data
  output$schmidt_selected <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    print(schmidt_diagram_se())
  })


  # download plunge rose
  output$download_schmidt_se1 <- downloadHandler(
    filename = function() {
      paste("schmidt_diagram", input$file_format_schmidt_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(schmidt_diagram_se())
      dev.off()
    }
  )


  # XY spatial projection with points
  projection_points_xy_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        data = selected_data(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xy_se()@extent@xmin,
          ortho_xy_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_se()@extent@ymin,
          ortho_xy_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with points
  projection_points_xz_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho_se) {
      xz <- ggplot2::ggplot(
        data = selected_data(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xz_se()@extent@xmin,
          ortho_xz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_se()@extent@ymin,
          ortho_xz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xz)
  })

  # YZ spatial projection with points
  projection_points_yz_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        data = selected_data(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_yz_se()@extent@xmin,
          ortho_yz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_se()@extent@ymin,
          ortho_yz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(yz)
  })

  # XY spatial projection with sticks
  projection_sticks_xy_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Y1,
          xend = selected_data()$X2,
          yend = selected_data()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Y1,
          xend = selected_data()$X2,
          yend = selected_data()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xy_se()@extent@xmin,
          ortho_xy_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_se()@extent@ymin,
          ortho_xy_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with sticks
  projection_sticks_xz_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho_se) {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Z1,
          xend = selected_data()$X2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Z1,
          xend = selected_data()$X2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xz_se()@extent@xmin,
          ortho_xz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_se()@extent@ymin,
          ortho_xz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(xz)
  })

  # YZ spatial projection with sticks
  projection_sticks_yz_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$Y1,
          y = selected_data()$Z1,
          xend = selected_data()$Y2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$Y1,
          y = selected_data()$Z1,
          xend = selected_data()$Y2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_yz_se()@extent@xmin,
          ortho_yz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_se()@extent@ymin,
          ortho_yz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(yz)
  })

  # xy plot with colored points or sticks
  output$projection_xy_selected_se <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(req(projection_points_xy_se()))
      } else {
        print(req(projection_sticks_xy_se()))
      }
    },
    height = 700,
    width = 700
  )

  # xz plot with colored points or sticks
  output$projection_xz_selected_se <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(projection_points_xz_se())
      } else {
        print(projection_sticks_xz_se())
      }
    },
    height = 300,
    width = 780
  )

  # yz plot with colored points or sticks
  output$projection_yz_selected_se <- renderPlot(
    {
      #req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(projection_points_yz_se())
      } else {
        print(projection_sticks_yz_se())
      }
    },
    height = 300,
    width = 780
  )


  # download xy projection
  output$download_xy_se1 <- downloadHandler(
    filename = function() {
      paste("XY projection", input$file_format_xy_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xy_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xy_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        print(projection_points_xy_se())
        dev.off()
      } else {
        print(projection_sticks_xy_se())
        dev.off()
      }
    }
  )


  # download xz projection
  output$download_xz_se1 <- downloadHandler(
    filename = function() {
      paste("XZ projection", input$file_format_xz_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xz_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xz_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        print(projection_points_xz_se())
        dev.off()
      } else {
        print(projection_sticks_xz_se())
        dev.off()
      }
    }
  )


  # download yz projection
  output$download_yz_se1 <- downloadHandler(
    filename = function() {
      paste("YZ projection", input$file_format_yz_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_yz_se1 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_yz_se1 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        print(projection_points_yz_se())
        dev.off()
      } else {
        print(projection_sticks_yz_se())
        dev.off()
      }
    }
  )


  # get selected data and add rayleigh tests
  summary_selected_data <- reactive({
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }


    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }

    # plotly_event_data <- event_data(event = 'plotly_selected', priority = "event")
    # req(plotly_event_data)
    selected <- selected_data() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)

    rayleigh_test <- round(data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- round(data.frame(spatial_bearing_double(selected)), 2)

    summary_data <- cbind(selected, rayleigh_test, rayleigh_double)
    return(summary_data)
  })

  #

  # summary data at sample scale
  summary_sample_se <- reactive({
    #req(input$user_data)
    req(input$data_type)

    data <- summary_selected_data()

    N <- nrow(data)
    IS <- round(mean(data$IS), 2)
    EL <- round(mean(data$EL), 2)
    PL <- round(mean(data$PL), 2)
    sd.IS <- round(sd(data$IS), 2)
    sd.EL <- round(sd(data$EL), 2)
    sd.PL <- round(sd(data$PL), 2)
    L <- round(mean(data$L), 2)
    Tr <- round(sum(data$R.p < 0.05) * 100 / nrow(data), 2)
    L.double <- round(mean(data$L.double), 2)
    Tr.double <- round(sum(data$R.p.double < 0.05) * 100 / nrow(data), 2)

    result <- data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
    return(result)
  })

  # summary statistics of selected data
  output$selected_sample_summary <- DT::renderDataTable(
    summary_sample_se(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_sample_se()),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )

  # get selected data
  output$click <- DT::renderDataTable(
    summary_selected_data(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_selected_data()),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )


  # reactive function for interactive projection
  # interactive_projection_plot <- reactive({
  #   data_to_plot <- benn_index_se()
  #
  #   colors <- data_to_plot %>%
  #     dplyr::select(IS, EL, PL) %>%
  #     dplyr::mutate(color_code = rgb(IS, EL, PL)) %>%
  #     dplyr::select(color_code)
  #   colors_p <- setNames(colors$color_code, data_to_plot$id)
  #
  #   if (input$axis == "XY") {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~X1, y = ~Y1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  #
  #     # fig <- ggplot(data_to_plot, aes(x = X1, y = Y1)) +
  #     #   geom_point()
  #     #
  #     # fig <- fig + coord_fixed(ratio = 1) + theme_minimal()
  #     #
  #     # fig <- ggplotly(fig)
  #
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   } else if (input$axis == "XZ") {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~X1, y = ~Z1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  #
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   } else {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~Y1, y = ~Z1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  #
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   }
  #   return(fig)
  # })

  ####################################################### from projection
  # reactive function for interactive projection
  interactive_projection_plot <- reactive({
    benn_index_se()

    if (input$axis == "XY") {
      fig <- ggplot(benn_index_se(), aes(x = X1, y = Y1)) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(
            round(data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        coord_fixed(ratio = 1) +
        theme_minimal()
    } else if (input$axis == "XZ") {
      fig <- ggplot(benn_index_se(), aes(x = X1, y = Z1)) +
        geom_point(
          shape = 21, col = "black",
          fill = rgb(
            round(data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        coord_fixed(ratio = 1) +
        theme_minimal()
    } else {
      fig <- ggplot(benn_index_se(), aes(x = Y1, y = Z1)) +
        geom_point(
          shape = 21, col = "black",
          fill = rgb(
            round(data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        coord_fixed(ratio = 1) +
        theme_minimal()
    }
    return(fig)
  })

  # render the plot
  # output$interactive_projection <- renderPlotly({
  #   interactive_projection_plot()
  # })
  output$interactive_projection <- renderPlot({
    interactive_projection_plot()
  })


  # get in a reactive function the selected data
  selected_data2 <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }

    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )

      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }

      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]

      return(list(benn))
    }

    # plotly_event_data <- event_data(event = "plotly_selected", priority = "event")
    # req(plotly_event_data)

    event_data <- brushedPoints(benn_index_se(), input$plot1_brush)
    req(event_data)

    selected <- filter(benn_index_se(), id %in% event_data$id) %>%
      dplyr::select(-c(EL, IS, PL))

    new_benn <- cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      mutate(PL = 1 - EL - IS)

    return(new_benn)
  })

  # plot the selected data in a new benn diagram
  selected_benn2_se2 <- reactive({
    req(input$plot1_brush)
    data_to_plot <- selected_data2() %>%
      dplyr::select(EL, IS, PL)

    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    recordPlot()
  })

  # plot the selected data in a new benn diagram
  output$selected_benn2 <- renderPlot({
    req(input$plot1_brush)
    print(selected_benn2_se2())
  })


  # download benn diagram
  output$download_benn_se2 <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_se2 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_se2 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(selected_benn2_se2())
      dev.off()
    }
  )

  # orientation rose diagram for selected data
  orientation_rose_se2 <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    d <- selected_data2()$orientation_pi + 180
    e <- c(selected_data2()$orientation_pi, d)
    f <- circular(e,
      type = "angles", units = "degrees",
      template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })

  # plot orientation_rose_se for selected data
  output$orientation_rose_selected2 <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    print(orientation_rose_se2())
  })


  # download orientation rose
  output$download_rose_o_se2 <- downloadHandler(
    filename = function() {
      paste("orientation_rose", input$file_format_rose_o_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_o_se2 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_o_se2 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(orientation_rose_se2())
      dev.off()
    }
  )


  # plunge rose diagram for selected data
  plunge_rose_se2 <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    k <- rose_diagram_plunge(selected_data2()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })


  # plot plunge_rose_c for selected data
  output$plunge_rose_selected2 <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    print(plunge_rose_se2())
  })


  # download plunge rose
  output$download_rose_p_se2 <- downloadHandler(
    filename = function() {
      paste("plunge_rose", input$file_format_rose_p_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_p_se2 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_p_se2 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(plunge_rose_se2())
      dev.off()
    }
  )


  # schmidt diagram for selected data
  schmidt_diagram_se2 <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    l <- schmidt_diagram(selected_data2()$bearing,
      selected_data2()$plunge,
      pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })


  # plot Schmidt diagram for selected data
  output$schmidt_selected2 <- renderPlot({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    print(schmidt_diagram_se2())
  })


  # download Schmidt
  output$download_schmidt_se2 <- downloadHandler(
    filename = function() {
      paste("schmidt_diagram", input$file_format_schmidt_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_se2 == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_se2 == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(schmidt_diagram_se2())
      dev.off()
    }
  )


  # get selected data and add rayleigh tests
  summary_selected_data2 <- reactive({
    req(input$plot1_brush)
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }


    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }

    selected <- selected_data2() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)

    rayleigh_test <- round(data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- round(data.frame(spatial_bearing_double(selected)), 2)

    summary_data <- cbind(selected, rayleigh_test, rayleigh_double)
    return(summary_data)
  })


  # summary data at sample scale
  summary_sample_se2 <- reactive({
    #req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)

    data <- summary_selected_data2()

    N <- nrow(data)
    IS <- round(mean(data$IS), 2)
    EL <- round(mean(data$EL), 2)
    PL <- round(mean(data$PL), 2)
    sd.IS <- round(sd(data$IS), 2)
    sd.EL <- round(sd(data$EL), 2)
    sd.PL <- round(sd(data$PL), 2)
    L <- round(mean(data$L), 2)
    Tr <- round(sum(data$R.p < 0.05) * 100 / nrow(data), 2)
    L.double <- round(mean(data$L.double), 2)
    Tr.double <- round(sum(data$R.p.double < 0.05) * 100 / nrow(data), 2)

    result <- data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
    return(result)
  })

  # summary statistics of selected data
  output$selected_sample_summary2 <- DT::renderDataTable(
    summary_sample_se2(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_sample_se2()),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )


  # get selected data
  output$click2 <- DT::renderDataTable(
    summary_selected_data2(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = nrow(summary_selected_data2()),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )

  ## --- Markdwon report ----

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste("report", sep = ".", switch(input$format,
        PDF = "pdf",
        HTML = "html",
        Word = "docx"
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      render_env <- new.env(parent = globalenv())
      render_env$benn_c <- benn_plot()
      render_env$xy_projection <- projection_points_xy_sm()
      render_env$xz_projection <- projection_points_xz_sm()
      render_env$yz_projection <- projection_points_yz_sm()
      render_env$benn_sm <- benn_sm_plot()

      rmarkdown::render(tempReport,
        output_file = file,
        params = params,
        envir = render_env
      )
    }
  )
} # end of server !!!

### --- Run the application ---
shinyApp(ui = ui, server = server)
