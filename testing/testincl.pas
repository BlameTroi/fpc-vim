{ Copyright (C) 1981 by Bell Laboratories, Inc., and Whitesmiths Ltd. }
{ Copyright 2023 by Troy Brumley, all rights reserved. }
{ globdefs (UCB) -- global constants, types and variables }

const

{ standard file descriptors. subscripts in open, etc. }
  STDIN = 1;		{ these are not to be changed }
  STDOUT = 2;
  STDERR = 3;

{ other io-related stuff }
  IOERROR = 0;	{ status values for open files }
  IOAVAIL = 1;
  IOREAD = 2;
  IOWRITE = 3;
  MAXOPEN = 10;	{ maximum number of open files }
