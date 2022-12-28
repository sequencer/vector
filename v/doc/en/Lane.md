

## Lane

### lane 主体

lane的主题需要处理的事项：

* 出读环逻辑
* 进读环逻辑
* 出写环逻辑
* 进写环逻辑
* 控制逻辑移动
* 控制逻辑试图完成一次访问 执行 写回 并自加计数器.(以状态机的形式， 后续追求性能可以考虑流水线)
* 控制逻辑访问rf的仲裁
* 控制逻辑使用执行单元的握手
* 新请求进入控制单元
* 吐结果给scheduler，可能会有等待scheduler的反馈
* scheduler的reduce结果写入
* 处理与lsu的接口
* 操作数的处理(todo)

### lane 控制逻辑的状态机

首先由于状态机会重复多次， 所以会有一个由指令决定的初始状态， 一个当前状态， 还有一个计数器。

初始状态会记录每一个循环里面需要做哪些事情。

当前状态记录还有哪些事情没干完。

计数器记录算到哪一个循环了， 每完成一个循环计数器加1, 当期状态重设成初始状态。

状态机需要监听的事项及控制信号名：

| 信号名       | 说明                                                         |
| ------------ | ------------------------------------------------------------ |
| sRead1       | 读 vs1                                                       |
| sRead2       | 读 vs2                                                       |
| sReadVD      | 读 vd， 给乘加或者跨lane读用                                 |
| wRead1       | 等待跨lane读的前半部分， 可能这前半部分在自己这里， 需要每个group index 自己判断 |
| wRead2       | 等待跨lane读的后半部分， 可能这前半部分在自己这里， 需要每个group index 自己判断 |
| (sCrossRead) | 跨lane的读， 代码里面复用用sRead2和sReadVD                   |
| wScheduler   | 等待scheduler的确认                                          |
| sExecute     | 去执行                                                       |
| sCrossWrite0 | 可能会有需要跨lane写的头                                     |
| sCrossWrite1 | 可能会有需要跨lane写的尾                                     |
| sSendResult0 | 将读到的头发送出去                                           |
| sSendResult1 | 将读到的尾发送出去                                           |
| sWrite       | 把结果写给rf，注意red 暂时是没有写的                         |

### 跨lane读写

注意： 第一个元操作数是vs2

w 		-> 		2sew = sew op sew

w.w 	-> 		2sew = 2sew op sew

n.w	-> 		sew = 2sew op sew

可以看出只有w前缀的会跨lane写， w后缀的会跨lane读

还有一种特殊的指令vrgather.vv 和 vrgatherei16.vv 需要特殊处理。

尤其vrgatherei16.vv也需要处理跨lane读， 而且更复杂， 暂时跳过这个处理。



对于正常的跨lane读， 当group index 为n时， sew的读取是第[8n, 8n + 8]的元素， 2sew读取的是[16n, 16n + 16]

可以看成是分别读的 group index = 2n的一组和  group index = 2n + 1 的一组

| 说明       | lane0 | lane1 | lane2 | lane3 | lane4 | lane5 | lane6 | lane7 |
| ---------- | ----- | ----- | ----- | ----- | ----- | ----- | ----- | ----- |
| sew        | n     | n + 1 | n + 2 | n + 3 | n + 4 | n + 5 | n + 6 | n + 7 |
| 2sew(2n +) | 0     | 1     | 2     | 3     | 4     | 5     | 6     | 7     |
|            | 8     | 9     | 10    | 11    | 12    | 13    | 14    | 15    |
| 目标lane   | 0     | 0     | 1     | 1     | 2     | 2     | 3     | 3     |
|            | 4     | 4     | 5     | 5     | 6     | 6     | 7     | 7     |

有一些特别的请求需要校正成vsew，比如vrgatherei16， lsu指令， 这种应该最大限度地读， 然后让musk unit或者lsu自己去拆（拼）。

由于2sew不会大于ELEN, 所以读环只需要一半就可以了
