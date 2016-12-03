type Minute = Int

data Time = Time {
  hours :: Int,
  minutes :: Minute
 } deriving (Show, Eq)

asMinutes :: Time -> Minute
asMinutes time_ = hours time_ * 60 + minutes time_

minus :: Time -> Time -> Minute
minus time1_ time2_ = asMinutes time1_ - asMinutes time2_
  
