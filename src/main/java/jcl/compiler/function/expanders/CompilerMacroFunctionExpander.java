/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function.expanders;

import jcl.lang.LispStruct;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;

public abstract class CompilerMacroFunctionExpander<O extends LispStruct> extends MacroFunctionExpander<O> implements CompilerMacroFunctionExpanderInter {

}
