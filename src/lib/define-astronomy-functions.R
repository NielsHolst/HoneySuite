# Convert degrees to radians
rad = function(degrees) {
  pi*degrees/180
}

# Convert radians to degrees
degrees = function(rad) {
  rad*180/pi
}

# Same as atan2 but with arguments in reverse order
ATAN2 = function(x,y) {
  atan2(y,x)
}

# Atmospheric refraction from solar elevation
refraction = function(b) {
  r = -20.772/tan(rad(b))
  ix1 = b>-0.575
  ix2 = b>5
  ix3 = b>85
  b1 = b[ix1]
  b2 = tan(rad(b[ix2]))
  r[ix1] = 1735 +b1*(-518.2+b1*(103.4+b1*(-12.79+b1*0.711))) 
  r[ix2] = 58.1/b2 - 0.07/b2^3 + 0.000086/b2^5 
  r[ix3] = 0 
  r/3600
}

# Date fraction of a day to date
date_time_fraction = function(date_time, fraction_of_a_day) {
  the_date = floor_date(date_time,unit="day") 
  new_hour = fraction_of_a_day*24
  the_date + new_hour*3600
}

# Calculates earth-solar variables for one date-time at one location
# returns a data frame with one row
solar_basics = function(latitude, longitude, time_zone, date_time) {
  cos_lat = cos(rad(latitude))
  sin_lat = sin(rad(latitude))
  tan_lat = tan(rad(latitude))
  days_since_2000 = floor(as.numeric(difftime(date_time, ymd("1999/12/31"), units="days")))
  days_since_1900 = 36525 + days_since_2000
  D2 = days_since_1900 
  TimePastLocalMidnight = (hour(date_time) + minute(date_time)/60 + second(date_time)/3600)/24   # in days
  E2 = TimePastLocalMidnight 
  JulianDay = D2+2415018.5+E2-time_zone/24
  F2 = JulianDay 
  JulianCentury = (F2-2451545)/36525
  G2 = JulianCentury
  GeomMeanLongSun = (280.46646+G2*(36000.76983 + G2*0.0003032)) %% 360
  I2 = GeomMeanLongSun
  GeomMeanAnomSun = 357.52911+G2*(35999.05029 - 0.0001537*G2)
  J2 = GeomMeanAnomSun
  EccentEarthOrbit = 0.016708634-G2*(0.000042037+0.0000001267*G2)
  K2 = EccentEarthOrbit
  SunEqofCtr = sin(rad(J2))*(1.914602-G2*(0.004817+0.000014*G2))+sin(2*rad(J2))*(0.019993-0.000101*G2)+sin(3*rad(J2))*0.000289
  L2 = SunEqofCtr
  SunTrueLong = I2+L2
  M2 = SunTrueLong
  SunTrueAnom = J2+L2
  N2 = SunTrueAnom
  SunRadVector = (1.000001018*(1-K2^2))/(1+K2*cos(rad(N2)))
  O2 = SunRadVector
  SunAppLong = M2-0.00569-0.00478*sin(rad(125.04-1934.136*F2))
  P2 = SunAppLong
  MeanObliqEcliptic = 23+(26+((21.448-G2*(46.815+G2*(0.00059-G2*0.001813))))/60)/60
  Q2 = MeanObliqEcliptic
  ObliqCorr = Q2+0.00256*cos(rad(125.04-1934.136*G2))
  R2 = ObliqCorr
  SunRtAscen = degrees(ATAN2(cos(rad(P2)),cos(rad(R2))*sin(rad(P2))))
  S2 = SunRtAscen
  SunDeclin = degrees(asin(sin(rad(R2))*sin(rad(P2))))
  T2 = SunDeclin
  var_y = tan(rad(R2/2))^2
  U2 = var_y
  EqofTime = 4*degrees(U2*sin(2*rad(I2))-2*K2*sin(rad(J2))+4*K2*U2*sin(rad(J2))*cos(2*rad(I2))-0.5*U2^2*sin(4*rad(I2))-1.25*K2^2*sin(2*rad(J2)))
  V2 = EqofTime
  HASunrise = degrees(acos(cos(rad(90.833))/(cos_lat*cos(rad(T2)))-tan_lat*tan(rad(T2))))
  W2 = HASunrise  
  SolarNoonLST = (720-4*longitude-V2+time_zone*60)/1440
  X2 = SolarNoonLST  # fraction of a day
  SunriseTimeLST = X2-W2*4/1440
  Y2 = SunriseTimeLST
  SunsetTimeLST = X2+W2*4/1440
  Z2 = SunsetTimeLST
  SunlightDuration = 8*W2
  AA2 = SunlightDuration 
  TrueSolarTime = (E2*1440+V2+4*longitude-60*time_zone) 
  true_solar_time_correction = rep(0,length(date_time))
  true_solar_time_correction[TrueSolarTime<0] = -1 
  true_solar_time_correction[TrueSolarTime>=1440] = 1 
  TrueSolarTime = TrueSolarTime %% 1440
  AB2 = TrueSolarTime
  HourAngle =  AB2/4+180 
  HourAngle[AB2/4>=0] = AB2/4-180
  AC2 = HourAngle
  SolarZenithAngle = degrees(acos(sin_lat*sin(rad(T2))+cos_lat*cos(rad(T2))*cos(rad(AC2))))
  AD2 = SolarZenithAngle
  SolarElevationAngle = 90-AD2
  AE2 = SolarElevationAngle 
  ApproxAtmosphericRefraction  = refraction(AE2)
  AF2 = ApproxAtmosphericRefraction 
  SolarElevationCorrected = AE2+AF2
  AG2 = SolarElevationCorrected
  SolarAzimuthAngle = rep(NA,length(date_time))  #(deg cw from N)
  ix1 = (AC2>0)
  ix2 = (AC2<=0)
  SolarAzimuthAngle[ix1] = (degrees(acos(((sin_lat*cos(rad(AD2[ix1])))-sin(rad(T2[ix1])))/(cos_lat*sin(rad(AD2[ix1])))))+180) %% 360
  SolarAzimuthAngle[ix2] = (540-degrees(acos(((sin_lat*cos(rad(AD2[ix2])))-sin(rad(T2[ix2])))/(cos_lat*sin(rad(AD2[ix2])))))) %% 360
  AH2 = SolarAzimuthAngle
  # Collate result
  data.frame(SolarDateTime=date_time_fraction(date_time, TrueSolarTime/24/60) + true_solar_time_correction*24*3600,
    SolarElevation=SolarElevationCorrected,
    SolarAzimuth=SolarAzimuthAngle,
    SolarNoon=date_time_fraction(date_time, SolarNoonLST),
    SunriseLocalTime=date_time_fraction(date_time, SunriseTimeLST),
    SunsetLocalTime=date_time_fraction(date_time, SunsetTimeLST),
    SunriseSolarTime=date_time_fraction(date_time, SunriseTimeLST-SolarNoonLST+0.5),
    SunsetSolarTime=date_time_fraction(date_time, SunsetTimeLST-SolarNoonLST+0.5),
    DayLength=SunlightDuration/60
  )
}

# Calculates earth-solar variables for a vector of date-times at one location; 
# returns a data frame with one row for each date-time
solar_time = function(latitude, longitude, time_zone, date_time_vector) {
  ldply(list(date_time_vector), function(x) solar_basics(latitude, longitude, time_zone, x))
}

# Get time of sunrise and sunset
sun = function(record, date) {
  # Use solar time at noon to get the date's sunrise and sunset
  T = solar_time(record$Latitude, record$Longitude, record$TimeZone, as.POSIXct(date) + 3600*12)
  data.frame(
    Rise = hours_since_midnight(T$SunriseSolarTime),
    Set  = hours_since_midnight(T$SunsetSolarTime)
  )
}
