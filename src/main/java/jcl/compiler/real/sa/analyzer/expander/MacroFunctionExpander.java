package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import jcl.lists.ListStruct;

/**
 * Defines an interface for MacroFunction lisp type
 */
public interface MacroFunctionExpander {

	LispStruct expand(ListStruct arg1);
}