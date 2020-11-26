module Lib
    ( someFunc
    ) where

import Tokens
import HappyParser
import PrettyPrinter

someFunc :: IO ()
someFunc = do
  orig <- getContents
  -- print $ happyParser $ alexScanTokens s


  let result = happyParser $ alexScanTokens orig
  case result of
    Error _ -> putStrLn "false, failed parsing"
    Ok    program -> do
      -- putStrLn "true"
      writeFile "prettyprint.c" (prettyPrint program)
      let cleanProgram = removeWhitespace (prettyPrint program)
          cleanOriginal = removeWhitespace orig

      if cleanOriginal == cleanProgram then 
        putStrLn "true"
      else
        putStrLn "false, failed printing"
      -- putStrLn cleanProgram
      -- putStrLn cleanOriginal
      -- print $ "RESULT: " ++ show (cleanProgram == cleanOriginal)
      -- print $ finddiff 0 (zip cleanOriginal cleanProgram)
      -- putStrLn $ prettyPrint program

      -- print program


removeWhitespace = filter (\c -> c `notElem` [' ', '\n'])


finddiff i ((a,b):xs) = if a /= b then i else finddiff (i+1) xs
finddiff i [] = i


-- orig = "bool x(bool eDn, bool cZ, bool vA) { { } } void CV(bool uY) { { if (V(true, -3)) { return true; return !true; return G = -(651||23); } else { } } return Ds; while (pd = 286) { O; { while (!((Nz = false)<Pfj())) { while (Ay) { } while (ekb = e) { return -884; return LAa(); } } { } return Q(En3 = UNF(748)); } } } bool BC(int n) { while (true>=40) { while (D) { } int hyz; 17; } while (G) { if (ZaJ = t2(!RWB, 2, vr = true)) { G5 = N(-vFB>=false); } else { { } { } } FOl(gs, e2x(5-(BaK = 408)), 847)>(--(false!=!61)<(E()>(pb = os))); if (-yJI) { { while (!!true) { C = true; } } } else { if (4) { return -M(-true, kX0 = B = 1); int yT; return true; } else { } if (!n(false, 632)) { } else { } } } bool T7J; }"

-- orig = "int main() {FOl(gs, e2x(5-(BaK = 408)), 847)>(--(false!=!61)<(E()>(pb = os)));}"
-- orig = "int main() {FOl()>(--(false!=!61)<(5>(pb = os)));}"


-- orig = "int nP2(bool i) { return b5h = CHn()<=TOg>true;}"



-- orig = "void S() { e0K(true>(OKg(!-eZu>=y))); }"

-- orig = "void H3(bool D) { if (!w()) { { if (jG3(7, Hm())) { bool G; } else { { while (yXs(A(6*((yMc = u)!=N()), tW(false, !LS1-(76+W3-(!d-((vc = 8)!=6)))*15, 25<=7), DoX)/F-false<true+474, JeZ = 3, R1I = !(Hj = wY!=!1))) { bool TC4; while (-338) { } } while (lp) { 7; { } { while (r) { } while (Mo) { { { return A = 7; } } return false; Zl; } bool p; } } } } } { } } else { } while (18) { bool Qo; bool tNy; return (Xb = !601/x(1, (Sk = Bc = (mAf = M)+!!(h4p = -(108/j))&&-((false==370+(pD7 = rvH))>=(Kp = sJ(GC(-c(true, false)!=B), !(true+-(lv<=gtb(-!!5)))))))>=true))<true; } }"



orig = "void UVU(bool J) { } bool i(int X, bool Ors) { return E; dp; } bool NII(bool s1, bool dY) { while (true) { { } if (MQW(e7v = c = hm(true))||aoo(aj)) { } else { } } if (5) { if (M) { -(E = false); int D; } else { return !-dV; { false; } } { } } else { return 146; { { } while (124) { true; 2; if (Oq) { while (!false) { bool H; while (OOG = Z) { if (true) { { { (pvp = true)!=---J47; if (o(t, w, -pm(56, v>=!-false, (msw==false)>=(FYB = V<false)))) { return GZ = false; } else { return Dj(ou(214), -3); 1; } } } 62; while (F()) { } } else { bool g; return -true; return (F = rt)<=true; } int md; while ((wfn = 85)||vy) { return Y(); Tco(c1(), 28, -G); 723; } } } } else { return !h; { } return -l5()*true; } } } int wS; } int wkN; }"