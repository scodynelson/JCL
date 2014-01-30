package jcl.types.conditions;

import jcl.types.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;

public enum ConditionType implements LispType, AtomicTypeSpecifier {

	TYPE_ERROR,
	SIMPLE_TYPE_ERROR,
	CONTROL_ERROR,
	PROGRAM_ERROR,
	UNDEFINED_FUNCTION,
	UNBOUND_SLOT,

	CONDITION,
	WARNING,
	STYLE_WARNING,
	SERIOUS_CONDITION,
	ERROR,
	CELL_ERROR,
	PARSE_ERROR,
	STORAGE_CONDITION,
	SIMPLE_ERROR,
	SIMPLE_CONDITION,
	SIMPLE_WARNING,

	UNBOUND_VARIABLE,

	PACKAGE_ERROR,

	ARITHMETIC_ERROR,
	DIVISION_BY_ZERO,
	FLOATING_POINT_INVALID_OPERATION,
	FLOATING_POINT_INEXACT,
	FLOATING_POINT_OVERFLOW,
	FLOATING_POINT_UNDERFLOW,

	FILE_ERROR,

	STREAM_ERROR,
	END_OF_FILE,

	PRINT_NOT_READABLE,

	READER_ERROR
}
