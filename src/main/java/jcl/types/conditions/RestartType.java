package jcl.types.conditions;

import jcl.types.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;

public enum RestartType implements LispType, AtomicTypeSpecifier {

	ABORT,
	CONTINUE,
	MUFFLE_WARNING,
	STORE_VALUE,
	USE_VALUE
}
