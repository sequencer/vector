#include <fmt/core.h>
#include <glog/logging.h>

#include "spikeevent.h"
#include "disasm.h"


bool SpikeEvent::get_issued() {
  return !_issue;
}

void SpikeEvent::commit() {
  // TODO: check this event is finished.
}

void SpikeEvent::issue() {
  assert(!get_issued());
  _issue = true;
}
uint32_t SpikeEvent::instruction() {
  return _inst;
}

uint64_t SpikeEvent::pc() {
  return _pc;
}

uint8_t SpikeEvent::vsew() {
  return _vsew;
}
uint8_t SpikeEvent::vlmul() {
  return _vlmul;
}
bool SpikeEvent::vma() {
  return _vma;
}
bool SpikeEvent::vta() {
  return _vta;
}
bool SpikeEvent::vill() {
  return _vill;
}
uint32_t SpikeEvent::vl() {
  return _vl;
}
uint16_t SpikeEvent::vstart() {
  return _vstart;
}
uint8_t SpikeEvent::vxrm() {
  return _vxrm;
}
bool SpikeEvent::vxsat() {
  return _vxsat;
}

void SpikeEvent::set_inst(uint32_t instruction){
  _inst = instruction;
}
void SpikeEvent::set_src1(uint32_t src1){
  _src1 = src1;
}
void SpikeEvent::set_src2(uint32_t src2){
  _src2 = src2;
}

void SpikeEvent::set_vsew(uint8_t vsew){
  _vsew = vsew;
}
void SpikeEvent::set_vlmul(uint8_t vlmul){
  _vlmul = vlmul;
}
void SpikeEvent::set_vma(bool vma){
  _vma = vma;
}
void SpikeEvent::set_vta(bool vta){
  _vta = vta;
}
void SpikeEvent::set_vl(uint32_t vl){
  _vl = vl;
}
void SpikeEvent::set_vxrm(uint8_t vxrm){
  _vxrm = vxrm;
}
void SpikeEvent::set_vstart(uint16_t vstart) {
  _vstart = vstart;
}

std::string SpikeEvent::disam() {
  return fmt::format("PC: {}, ASM: {:032b}, DISASM: {}", _pc, _inst, _proc.get_disassembler()->disassemble(_inst));
}

void SpikeEvent::log_reset() {
  // clear state for difftest.
  _proc.get_state()->log_reg_write.clear();
  _proc.get_state()->log_mem_read.clear();
  _proc.get_state()->log_mem_write.clear();
}

void SpikeEvent::assign_instruction(uint32_t instruction) {
  _pc = _proc.get_state()->pc;
  _inst = instruction;
}
// TODO
void SpikeEvent::get_mask() {
  // get mask from v0
  _mask = _proc.VU.elt<uint8_t>(0, 0);
}
void SpikeEvent::log() {
  auto state = _proc.get_state();
  auto& regs = state->log_reg_write;
  auto& loads = state->log_mem_read;
  auto& stores = state->log_mem_write;
  for (auto reg : regs) {
    // in spike, log_reg_write is arrange:
    // xx0000 <- x
    // xx0001 <- f
    // xx0010 <- vreg
    // xx0011 <- vec
    // xx0100 <- csr
    if ((reg.first & 0xf) == 2) {
      // TODO: based on VLMUL, SEW, set _vrf
      continue;
    }
  }
  for (auto mem_write : state->log_mem_write) {

  }
  for (auto mem_read : state->log_mem_write) {

  }
}