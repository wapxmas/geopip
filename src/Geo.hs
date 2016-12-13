module Geo (
module Geo, GEO.Point(..), GEO.pt
) where

import qualified Geo.Computations as GEO

type GEOPoint      = GEO.Point

isPointInPolygon :: [GEOPoint] -> GEOPoint -> Bool
isPointInPolygon [] _ = False
isPointInPolygon ps pt =
  let
    p0 = head ps
    pm = last ps
    rs =
      if p0 == pm
        then isPointInPolygon'Res (init ps) p0
        else isPointInPolygon'Res ps p0
  in
    sum rs /= 0
  where
    isPointInPolygon'Calc :: GEOPoint -> GEOPoint -> Int
    isPointInPolygon'Calc p0 p1
      | GEO.pntLat p0 <= GEO.pntLat pt &&
        GEO.pntLat p1 > GEO.pntLat pt &&
        geoIsLeft p0 p1 pt > 0 = 1
      | GEO.pntLat p0 > GEO.pntLat pt &&
        GEO.pntLat p1 <= GEO.pntLat pt &&
        geoIsLeft p0 p1 pt < 0 = -1
      | otherwise = 0

    isPointInPolygon'Res :: [GEOPoint] -> GEOPoint -> [Int]
    isPointInPolygon'Res [] _ = []
    isPointInPolygon'Res pss lps =
      let
        (nxps, p0, p1) = isPointInPolygon'Get pss lps
      in
          isPointInPolygon'Calc p0 p1
        : isPointInPolygon'Res nxps lps

    isPointInPolygon'Get
      :: [GEOPoint] -> GEOPoint -> ([GEOPoint], GEOPoint, GEOPoint)
    isPointInPolygon'Get [] lps = ([], lps, lps)
    isPointInPolygon'Get (p0:p1:pss) _ = (p1:pss, p0, p1)
    isPointInPolygon'Get (p0:pss) lps = (pss, p0, lps)

    geoIsLeft :: GEOPoint -> GEOPoint -> GEOPoint -> Int
    geoIsLeft p0 p1 p2 =
      let
        c :: Double
        c = ((GEO.pntLon p1 - GEO.pntLon p0) * (GEO.pntLat p2 - GEO.pntLat p0)
           - (GEO.pntLon p2 - GEO.pntLon p0) * (GEO.pntLat p1 - GEO.pntLat p0))

        res :: Int
        res
          | c > 0 = 1
          | c < 0 = -1
          | otherwise = 0
      in
        res
