## 	VRF

### 1. 描述

* 这是一个 ELEN = 32 的向量扩展单元, 所以vrf的宽度是 32.
* sew 会有 0 , 1, 2 三种, 因为对于sew=0的支持, 粒度是 8.
* vrf 是用 2读1写的 regfile 搭.
* v0需要用寄存器搭.
* 会有乘加需要用到三个源操作数, 我们需要打乱rf的排列减少访问冲突.
* 由于要做chaining, 所以需要一个更细粒度的raw检查.

### 2. 特别说明

#### 2.1 寻址

已有的信息: lane size, reg index, group index, eew, lane index(可能不会被用到)

计算信息: bank enable, rf index

一个正常的寄存器sew=0时在一个lane里面的摆放:

![](assets/rvv-lane_reg.drawio.svg)

可以看到一个寄存器在 rf 里面是占四行的, 需要注意的是序号相临的两个元素之间并不是相邻的, 在图中的例子里他们差8 Byte.

index的高位不变,一直是reg index * 4

index的低位:

* sew = 0 -> group index >> 2
* sew = 1 -> group index >> 1
* sew = 2 -> group index >> 0

group index >> (max sew - sew)



bank enable 先用查表吧

#### 2.2 打乱

乘加需要用到三个源操作数, 每一次操作需要的是三个源操作数的同一个位置, 在2.1里面可以看出来, 他们在rf的同一列, 而我们的rf是双读口的,这会导致我们无法在一个周期将三个操作数全读出来, 我们采用打乱寄存器排列的方式来减少这个冲突.

![](assets/rvv-rf-sew0.drawio.svg)

图画出了在sew=0时多个寄存器前4个数据块的排列, 暂时是很规则的斜排.

现在是 (group index, reg index, sew) => bank enable的一次查表.

后续sequencer准备用一个能动态调整的csr来打乱这一个排序, 变成(group index, reg index, sew, csr) => bank enable 的一次查表.

#### 2.3 chaining

如果前一个指令需写某个寄存器 Vx, 而下一个指令需要读这一个寄存器, 我们可以不等到第一个指令完全执行完, 我们可以更细粒度地去处理这一个写后读.

这有一些问题需要讨论:

* 由于 rf 的读冲突, 能同时执行的指令不会太多.
* 需要记录每条指令的顺序和写寄存器的编号.
* 每次有写都需要记录,如果顺序写, 只需要记录index, 但是会有乱序的写.(乱的不做chaining)
* 读的时候需要判断是否有raw, 如果有, 需要根据上面的记录去判断是否已经被写过了.
* 存在有可能发生exception的指令不能继续抢先执行后续指令.
* 多个指令读 rf 可能需要merge.

### 3. 实现

#### 3.1 rf主体

每一个lane有4 (elen / 8) 个rf, 为了方便替换,我们将它写成小模块.

整理一下参数:

宽: 8

深度: vlen * 32 / lane / (elen / 8) / 8 = 128

2 读 1 写

v0的部分不做特别处理.

#### 3.2 外壳

todo
