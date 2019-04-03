/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.TStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.type.FloatType;
import jcl.type.SingleFloatType;

/**
 * Interface for defining global reader variables for the JCL system.
 */
public interface ReaderVariables {

	/**
	 * A {@link RadixVariable} type variable used to determine in which radix base to read numeric values.
	 */
	VariableStructImpl<IntegerStruct> READ_BASE = new RadixVariable("*READ-BASE*", GlobalPackageStruct.COMMON_LISP);

	/**
	 * Determine the format in which to read float numeric values.
	 */
	VariableStructImpl<FloatType> READ_DEFAULT_FLOAT_FORMAT = VariableStructImpl.valueOf("*READ-DEFAULT-FLOAT-FORMAT*", GlobalPackageStruct.COMMON_LISP, SingleFloatType.INSTANCE);

	/**
	 * Determines whether or not the read operation should be evaluated where *read-eval* is used.
	 */
	VariableStructImpl<BooleanStruct> READ_EVAL = VariableStructImpl.valueOf("*READ-EVAL*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);

	/**
	 * Determines whether or not the read operation should suppress the read in lisp tokens.
	 */
	VariableStructImpl<BooleanStruct> READ_SUPPRESS = VariableStructImpl.valueOf("*READ-SUPPRESS*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);

	/**
	 * The global {@link ReadtableStruct} denoting the default readtable to be used throughout the JCL system.
	 */
	VariableStructImpl<ReadtableStruct> READTABLE = VariableStructImpl.valueOf("*READTABLE*", GlobalPackageStruct.COMMON_LISP, ReadtableStruct.toReadtable());
}
