let _ =
  let pi=acos (-1.) in
  let a=2. in
  let b=1. in
  let a2=a*.a and b2=b*.b in
  let c=sqrt (a2-.b2) in
  let e=c/.a and p=b2/.a in
  let curv teta=p/.(1.+.e*.cos teta) in
  for i=0 to 360 do
    let fi=(float i)/.180.*.pi in
    let rho=curv fi in
    Printf.printf "%f %f\n" (rho*.cos fi) (rho*.sin fi)
  done;

  
