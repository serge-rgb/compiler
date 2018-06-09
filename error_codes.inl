#pragma once

enum ErrorCode {
  Ok = 0,

  Fail = 1,

  IntParse,
  CouldNotAssemble,
  CouldNotLink,
} typedef ErrorCode;
