# part2

Generated lexer and parser using the libraries Alex and Happy.

# Grammar of the language
```
Program ::= Decl*

Decl ::= Type id ( FormalList ) { Stmt* }
       | void id ( FormalList ) { Stmt* }

FormalList ::= Type id (, Type id)* | λ

Type ::= int | bool

Stmt ::= { Stmt* }
      |  if ( Expr ) Stmt
      |  if ( Expr ) Stmt else Stmt
      |  while ( Expr ) Stmt
      |  return Expr ;
      |  return ;
      |  Expr ;
      |  Type id ;

Expr ::= int
      |  true
      |  false
      |  Expr bop Expr
      |  id = Expr
      |  id
      |  uop Expr
      |  id ( ExprList )
      |  ( Expr )

ExprList ::= Expr (, Expr)* | λ
```

# Mine (err at 387)
boolx(booleDn,boolcZ,boolvA){{}}voidCV(booluY){{if(V(true,-3)){returntrue;return!true;returnG=-651||23;}else{}}returnDs;while(pd=286){O;{while(!(Nz=false)<Pfj()){while(Ay){}while(ekb=e){return-884;returnLAa();}}{}returnQ(En3=UNF(748));}}}boolBC(intn){while(true>=40){while(D){}inthyz;17;}while(G){if(ZaJ=t2(!RWB,2,vr=true)){G5=N(-vFB>=false);}else{{}{}}FOl(gs,e2x(5-(BaK=408)),847)>--false!=!61<E()>(pb=os);if(-yJI){{while(!!true){C=true;}}}else{if(4){return-M(-true,kX0=B=1);intyT;returntrue;}else{}if(!n(false,632)){}else{}}}boolT7J;}

# Orig
boolx(booleDn,boolcZ,boolvA){{}}voidCV(booluY){{if(V(true,-3)){returntrue;return!true;returnG=-(651||23);}else{}}returnDs;while(pd=286){O;{while(!((Nz=false)<Pfj())){while(Ay){}while(ekb=e){return-884;returnLAa();}}{}returnQ(En3=UNF(748));}}}boolBC(intn){while(true>=40){while(D){}inthyz;17;}while(G){if(ZaJ=t2(!RWB,2,vr=true)){G5=N(-vFB>=false);}else{{}{}}FOl(gs,e2x(5-(BaK=408)),847)>(--(false!=!61)<(E()>(pb=os)));if(-yJI){{while(!!true){C=true;}}}else{if(4){return-M(-true,kX0=B=1);intyT;returntrue;}else{}if(!n(false,632)){}else{}}}boolT7J;}


# The above and below are exclusive, solving one breaks the other.

# Mine (err at 84)
intnP2(booli){return185;booldzc;if(18){if(-oP){while(65){if(jUR=true-Co){returnb5h=(CHn()<=TOg)>true;-((G=U1=true)!=863);}else{}}while(!(true<(true<=false))){}intK;}else{}while(21&&!false){if(OKA(-EK,true,false)){if(n5(false,Ww=J)){false;-KS;intL;}else{}}else{}{{{intwMf;}if(-true){return656;}else{}}booldO;}}if(!-!(false+725)){return26;}else{}}else{}}inttW(boole,boolewr,boolLT){}boolm(boolIl,boolAF){while(Y()){cF=-pl;qO=true;}while(-b2(!true,-!(h5u(AQ=5<=!(F(U,-80)||S(sM,b)),-I8(),1)>=((b=Jt())<-(uXh((Cq=4)-K,R1k=!!K3)-true))),30)){return(J6=Sqo=Xb(L))+(KZY=!(xkI=cY));if(true!=(HTq=74)){if(862){while(LH(hk=H=ndG(k,false,L(Y(2,551,true))),hw)){intTw;}}else{}{}-true;}else{returnFsj&&Zuq(!((rm=DB1=!(a(!7)==true<qPJ()*!(w0=8))+true)&&(n=Q)));}}intK7;}

# Orig
intnP2(booli){return185;booldzc;if(18){if(-oP){while(65){if(jUR=true-Co){returnb5h=CHn()<=TOg>true;-((G=U1=true)!=863);}else{}}while(!true<(true<=false)){}intK;}else{}while(21&&!false){if(OKA(-EK,true,false)){if(n5(false,Ww=J)){false;-KS;intL;}else{}}else{}{{{intwMf;}if(-true){return656;}else{}}booldO;}}if(!(-!false+725)){return26;}else{}}else{}}inttW(boole,boolewr,boolLT){}boolm(boolIl,boolAF){while(Y()){cF=-pl;qO=true;}while(-b2(!true,-!(h5u(AQ=5<=(!F(U,-80)||S(sM,b)),-I8(),1)>=((b=Jt())<-(uXh((Cq=4)-K,R1k=!!K3)-true))),30)){return(J6=Sqo=Xb(L))+(KZY=!(xkI=cY));if(true!=(HTq=74)){if(862){while(LH(hk=H=ndG(k,false,L(Y(2,551,true))),hw)){intTw;}}else{}{}-true;}else{returnFsj&&Zuq(!((rm=DB1=(!a(!7)==true<qPJ()*!(w0=8))+true)&&(n=Q)));}}intK7;}




# Mine (err at 146)
intup(intOp,intc,boolJDI){GvP-(BxB=753);inth1;}voidd(intPG,intoxL,intn2i){false;while(-132){88;if(224){while(Cn){return78;}returnjYc(JnN(),50,(Ef=(E=!r))<5)<=false;while(mhD){boolVMD;returnV6+((p=(XL=!8))!=!45)<he;}}else{}}while(--(hZ(false,Nm=-(at=(jA=(DoQ=782>aS(FnA=(wG2=(i=m0F(!!gfr(X<dW),P=(c=140&&640)))))))),x(false))*(true<=(v=4)))){return5;}}boold(boolTNM){while(dTX(L=(D=Pj7(yj(32!=sG(p=a,false,hwO=X(yc(),L5M=wLZ(45))))<=(false<!!-((lf=bN)/MTa))<-(false*true)))&&(Z=odI(bj8=oA()<(6<=FoK),i(false,-(FP||(bWH=true)),42),(h=-FN)!=IOU())))){}intZ;}

# Orig
intup(intOp,intc,boolJDI){GvP-(BxB=753);inth1;}voidd(intPG,intoxL,intn2i){false;while(-132){88;if(224){while(Cn){return78;}returnjYc(JnN(),50,(Ef=E=!r)<5)<=false;while(mhD){boolVMD;returnV6+((p=XL=!8)!=!45)<he;}}else{}}while(--(hZ(false,Nm=-(at=jA=DoQ=782>aS(FnA=wG2=i=m0F(!!gfr(X<dW),P=c=140&&640))),x(false))*(true<=(v=4)))){return5;}}boold(boolTNM){while(dTX(L=(D=Pj7(yj(32!=sG(p=a,false,hwO=X(yc(),L5M=wLZ(45))))<=(false<!!(-(lf=bN)/MTa))<-false*true))&&(Z=odI(bj8=oA()<(6<=FoK),i(false,-(FP||(bWH=true)),42),(h=-FN)!=IOU())))){}intZ;}



# Mine (err at 536)
voidUVU(boolJ){}booli(intX,boolOrs){returnE;dp;}boolNII(bools1,booldY){while(true){{}if(MQW(e7v=c=hm(true))||aoo(aj)){}else{}}if(5){if(M){-(E=false);intD;}else{return!-dV;{false;}}{}}else{return146;{{}while(124){true;2;if(Oq){while(!false){boolH;while(OOG=Z){if(true){{{(pvp=true)!=---J47;if(o(t,w,-pm(56,v>=!-false,(msw==false)>=(FYB=V<false)))){returnGZ=false;}else{returnDj(ou(214),-3);1;}}}62;while(F()){}}else{boolg;return-true;return(F=rt)<=true;}intmd;while((wfn=85)||vy){returnY();Tco(c1(),28,-G);723;}}}}else{return!h;{}return-(l5()*true);}}}intwS;}intwkN;}

# Orig
voidUVU(boolJ){}booli(intX,boolOrs){returnE;dp;}boolNII(bools1,booldY){while(true){{}if(MQW(e7v=c=hm(true))||aoo(aj)){}else{}}if(5){if(M){-(E=false);intD;}else{return!-dV;{false;}}{}}else{return146;{{}while(124){true;2;if(Oq){while(!false){boolH;while(OOG=Z){if(true){{{(pvp=true)!=---J47;if(o(t,w,-pm(56,v>=!-false,(msw==false)>=(FYB=V<false)))){returnGZ=false;}else{returnDj(ou(214),-3);1;}}}62;while(F()){}}else{boolg;return-true;return(F=rt)<=true;}intmd;while((wfn=85)||vy){returnY();Tco(c1(),28,-G);723;}}}}else{return!h;{}return-l5()*true;}}}intwS;}intwkN;}