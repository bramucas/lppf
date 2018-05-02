 :- owns(X0,X1,X2),owns(X0,X1,X3),X2!=X3.
 :- controls(X0,X1,X2,X3),controls(X0,X1,X2,X4),X3!=X4.
 :- sum(X0,X1,X2),sum(X0,X1,X3),X2!=X3.
company(c1).
company(c2).
company(c3).
%owns(c1,c2,51).
owns(c2,c3,51).
owns(c3,c2,1).
controls(C1,C1,C2,X0) :- owns(C1,C2,X0).
controls(C1,C2,C3,X0) :- controls(C1,C2),company(C3),owns(C2,C3,X0).
controls(C1,C2,C3,0) :- company(C1),company(C2),company(C3),not p0(C1,C2,C3).
p0(C1,C2,C3) :- company(C1),company(C2),company(C3),X0!=0,controls(C1,C2,C3,X0).
sum(C1,C3,X0) :- X0=X1+X4,X1=X2+X3,controls(C1,c1,C3,X2),controls(C1,c2,C3,X3),controls(C1,c3,C3,X4).
controls(C1,C3) :- X0>50,sum(C1,C3,X0).