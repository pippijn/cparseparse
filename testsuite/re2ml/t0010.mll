rule token = parse
| "/*" (([^ '*'] | "*"* [^ '*' '/'])*) "*"+ "/"                    { token lexbuf }
