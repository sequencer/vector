## LSU

#### 1. spec记录

* unit-stride 是连续的访存,也就是间隔是eew
* constant-strided 间隔是记录在 rs2 里面的定值.
* indexed 是直接基于基地址的offset存在vs2里面
* offset 在加base之前做0扩展,如果超过了就直接切去非有效位.
* unit-stride 和 stride都是无序的, index两种都有.
* index的eew作用于offset, data依然沿用原来的, 剩下的两种eew是编码数据的(待确认)
* constant-strided 不用x0作为rs2,但是x[rs2] = 0的时候虽然地址不变,但是每个有效的element都需要有访存操作.
* ls whole register 有自己的evl = nf * vlen / eew, evl 会代替vl作为控制.

#### 2. 实现记录

* lsu 下面挂的分bank的l2, **用低位地址来分bank？**
* lsu指令需要等到标量st buff 清空才开始执行
* 有需要顺序执行的只能一个一个执行
* 需要等待第一个异常的需要先等待第一个回应
* 对于同一个指令来说，有自己的idRange, 回应的时候再用这个id来算应该写到寄存器的哪个位置
* 对于不同的指令来说,后面的不能超过前面的进度以免对同一个地址的操作乱序
* 有写类型的指令不需要从rf读数据，需要处理lane里面控制信号的沟通

