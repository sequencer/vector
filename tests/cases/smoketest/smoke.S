.global _start
_start:
    li a0, 0x200 # MSTATUS_VS & (MSTATUS_VS >> 1)
    csrs mstatus, a0
    csrwi vcsr, 0

    addi x1, x1, 1
    vsetivli x0, 16, e8, m1, ta, ma
    lui x30, 1
    auipc x31, 0

    vmv.v.i v1, 0
    vmv.v.i v5, 0

add_test:
    vadd.vi v0, v1, 10
    vadd.vv v2, v1, v1
    vadd.vi v1, v1, 7, v0.t
    vadd.vx v1, v1, x1

chaining_test:
    vadd.vx v1, v1, x1
    vxor.vi v7, v5, 7
    vsll.vi v1, v1, 1

ld_test:
    vle8.v v4, (x30)

exit:
    # Write msimend to exit simulation.
    csrwi 0x7cc, 0

will_not_be_executed:
    vadd.vv v2, v1, v1
