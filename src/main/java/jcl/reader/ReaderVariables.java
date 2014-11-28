/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.RadixVariable;
import jcl.reader.struct.ReadtableStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.symbols.Variable;
import jcl.types.SingleFloat;

public interface ReaderVariables {

	Variable<IntegerStruct> READ_BASE = new RadixVariable("*READ-BASE*", GlobalPackageStruct.COMMON_LISP);
	Variable<jcl.types.Float> READ_DEFAULT_FLOAT_FORMAT = new Variable<>("*READ-DEFAULT-FLOAT-FORMAT*", GlobalPackageStruct.COMMON_LISP, SingleFloat.INSTANCE);
	Variable<BooleanStruct<?>> READ_EVAL = new Variable<>("*READ-EVAL*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	Variable<BooleanStruct<?>> READ_SUPPRESS = new Variable<>("*READ-SUPPRESS*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	Variable<ReadtableStruct> READTABLE = new Variable<>("*READTABLE*", GlobalPackageStruct.COMMON_LISP, new ReadtableStruct());
}
