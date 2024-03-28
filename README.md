# ASG

A simple pseudo random generator which has an extremely long period. This serves as a demonstration of the power of SpinalHDL. The project uses the Rings library \[1\] to check whether a given polynomial is primitive. For this purpose, complicated algorithms such as the quadratic sieve (to factorize numbers) and arithmetic for finite fields are used. Once the calculation is complete, an efficient hardware description (Verilog and VHDL) is generated.

\[1\] Stanislav Poslavsky, _Rings: An efficient Java/Scala library for polynomial rings_, Computer Physics Communications, Volume 235, 2019, Pages 400-413, [doi:10.1016/j.cpc.2018.09.005](https://doi.org/10.1016/j.cpc.2018.09.005)
