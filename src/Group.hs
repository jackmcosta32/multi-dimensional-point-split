module Group where

import Helpers

handleMetric metric p0 ps = do
  let costs = metric p0 ps
  let maybeIndex = minValueIndex costs

  case maybeIndex of
    Just index -> return index
    Nothing -> error "Something went wrong :/"

group metric k ps = do
  let (p0 : pss) = ps
  let result = [p0]

  let costs = metric p0 pss
  let maybeIndex = minValueIndex costs

  case maybeIndex of
    Just index -> result ++ [pss!!index]
    Nothing -> error "Something went wrong :/"

  return result

