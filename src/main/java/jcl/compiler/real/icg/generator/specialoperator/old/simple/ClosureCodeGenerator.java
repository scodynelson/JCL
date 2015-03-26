/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import java.util.List;
import java.util.Optional;

import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.PositionAllocation;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.collections4.CollectionUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class ClosureCodeGenerator implements CodeGenerator<Environment> {

	@Override
	public void generate(final Environment input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Closure closureSetBody = input.getClosure();
		final int numParams = closureSetBody.getBindings().size(); // remove :closure and (:depth . n) from contention

		if (numParams > 0) {
			// keep a copy of the 'this' reference
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "getClosure", "()Llisp/extensions/type/Closure;", false);
			mv.visitLdcInsn(numParams);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/extensions/type/Closure$Factory", "newInstance", "(Llisp/extensions/type/Closure;I)Llisp/extensions/type/Closure;", false);
			// have a closure object on the stack
			//push it onto the closure stack
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "addClosure", "(Llisp/extensions/type/Closure;)Llisp/extensions/type/Closure;", false);
			mv.visitInsn(Opcodes.POP);
		}

		final Closure closureStuff = input.getClosure();
		final List<EnvironmentParameterBinding> bindings = input.getLexicalBindings();
		// (:closure (:depth . n) (x ....) (y ....) ...)
		// if there is one, allocate the object
		if (CollectionUtils.isNotEmpty(bindings) && (closureStuff != null)) {
			// get the top closure object
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/function/FunctionBaseClass", "getClosure", "()Llisp/extensions/type/Closure;", false);
			mv.visitInsn(Opcodes.DUP);
			// run the list of variables
			//TODO handle parameters that are special variables
			for (final Binding<?> binding : bindings) {
				final SymbolStruct<?> variable = binding.getSymbolStruct();
				final Optional<ClosureBinding> closureEntry = closureStuff.getBinding(variable);
				if (closureEntry.isPresent()) {
					// this entry is a closure
					// Since this is a lambda, it's a parameter. So put the value into the closure
					final PositionAllocation allocation = (PositionAllocation) binding.getAllocation();
					final int param = allocation.getPosition();
					// now where does it go
					ClosureBinding closureBinding = closureEntry.get();
					final int position = closureBinding.getPosition();
					mv.visitLdcInsn(position); // index
					mv.visitLdcInsn(0);                   // nesting (current one)
					mv.visitVarInsn(Opcodes.ALOAD, param);      // value from the arg list
					mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/extensions/type/Closure", "setBindingAt", "(IILjava/lang/Object;)V", true);
					// the closure is left on the stack
					// dup it for the rest loop, except the last time around
					mv.visitInsn(Opcodes.DUP);
				}
			}
			mv.visitInsn(Opcodes.POP2);
			// drop the remaining closure reference from the stack
			// for each variable, gen code to get the value and store in the closure
		}
	}
}
