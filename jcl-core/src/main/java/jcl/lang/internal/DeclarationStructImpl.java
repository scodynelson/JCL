package jcl.lang.internal;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public final class DeclarationStructImpl extends SymbolStructImpl {

	// TODO: These are NOT declared in the "COMMON_LISP" package. They probably exist in the "SYSTEM" package instead. Have to work this in...
	public static final DeclarationStructImpl IGNORE = new DeclarationStructImpl("IGNORE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl IGNORABLE = new DeclarationStructImpl("IGNORABLE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl DYNAMIC_EXTENT = new DeclarationStructImpl("DYNAMIC-EXTENT", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl TYPE = new DeclarationStructImpl("TYPE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl INLINE = new DeclarationStructImpl("INLINE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl NOTINLINE = new DeclarationStructImpl("NOTINLINE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl FTYPE = new DeclarationStructImpl("FTYPE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl DECLARATION = new DeclarationStructImpl("DECLARATION", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl OPTIMIZE = new DeclarationStructImpl("OPTIMIZE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl SPECIAL = new DeclarationStructImpl("SPECIAL", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl COMPILATION_SPEED = new DeclarationStructImpl("COMPILATION-SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl DEBUG = new DeclarationStructImpl("DEBUG", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl SAFETY = new DeclarationStructImpl("SAFETY", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl SPACE = new DeclarationStructImpl("SPACE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl SPEED = new DeclarationStructImpl("SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStructImpl DOCUMENTATION = new DeclarationStructImpl("%DOCUMENTATION", GlobalPackageStruct.SYSTEM);

	public static final DeclarationStructImpl JAVA_CLASS_NAME = new DeclarationStructImpl("%JAVA-CLASS-NAME", GlobalPackageStruct.SYSTEM);

	public static final DeclarationStructImpl LISP_NAME = new DeclarationStructImpl("%LISP-NAME", GlobalPackageStruct.SYSTEM);

	private DeclarationStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		setValue(this);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(this, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final Environment currentEnvironment = generatorState.getCurrentEnvironment();

		final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(this);
		final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(this);

		if (hasLexicalBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC,
			                   true);
		} else if (hasDynamicBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_DESC,
			                   true);
		}
	}
}
