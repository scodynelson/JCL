package jcl.compiler.real.icg.generator.specialoperator.old.compiler;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.FunctionCallCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

//@Component
public class TailRecursionCodeGenerator implements CodeGenerator<ListStruct> {

	// the list is of the form (%tail-recursion fn-symbol arg...)

	//	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	//	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		// drop the special operator
		final ListStruct restOfList = input.getRest();
		// set up the proper function object (this)
		genCodeTailRecursionSetup((SymbolStruct) restOfList.getFirst(), mv);
		// now set up the rest of the call just like any other fn call
		final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
		try {
			classBuilder.setAcceptsMultipleValues(false);
//			functionCallCodeGenerator.generate(restOfList, classBuilder);
		} finally {
			classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
		}
	}

	/**
	 * This method handles a simple tail recursion. Instead of looking up a function,
	 * either by symbol name or from the list of std CL functions, the method
	 * just sets up to call the enclosing function's funcall or apply method. Since the
	 * enclosing function is the current object, the method only generates an ALOAD 0 -
	 * the reference to 'this'.
	 *
	 * @param sym
	 * 		sym
	 * @param mv
	 * 		classBuilder
	 */
	private static void genCodeTailRecursionSetup(final SymbolStruct<?> sym, final MethodVisitor mv) {
		mv.visitVarInsn(Opcodes.ALOAD, 0);
	}
}
