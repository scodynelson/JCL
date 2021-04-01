/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.defstruct;

import java.util.Deque;
import java.util.List;
import java.util.Map;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.NILStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StructureClassStruct;
import lombok.Getter;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

@Getter
public class DefstructStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct structureSymbol;
	private final StructureClassStruct includeStructureClass;
	private final SymbolStruct defaultConstructorSymbol;
	private final SymbolStruct printerSymbol;
	private final List<SymbolStruct> slots;

	public DefstructStruct(final SymbolStruct structureSymbol, final StructureClassStruct includeStructureClass,
	                       final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                       final List<SymbolStruct> slots) {
		super("defstruct");
		this.structureSymbol = structureSymbol;
		this.includeStructureClass = includeStructureClass;
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
		this.slots = slots;
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing
	}

	private static final String STRUCT_CLASSES_PACKAGE = "jcl/structures/struct/classes/";

	private static final String STRUCT_OBJECTS_PACKAGE = "jcl/structures/struct/objects/";

	private static final String STRUCTURE_CLASS_POSTFIX = "StructureClass";

	private static final String STRUCTURE_OBJECT_POSTFIX = "StructureObject";

	private static final String INIT_SLOTS_MAP_METHOD_NAME = "initSlotsMap";

	private static final String INIT_SLOTS_MAP_METHOD_DESC = "()V";

	private static final String SLOTS_FIELD = "slots";

	private static final String STRUCTURE_OBJECT_INIT_SCS_SS_SOS_METHOD_DESC = "(Ljcl/lang/classes/StructureClassStruct;Ljcl/lang/SymbolStruct;Ljcl/lang/StructureObjectStruct;)V";

	private static final String STRUCTURE_CLASS_INIT_METHOD_DESC = "(Ljcl/lang/SymbolStruct;Ljcl/lang/SymbolStruct;Ljcl/lang/SymbolStruct;)V";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME = "newInstance";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC = "()Ljcl/lang/StructureObjectStruct;";

	private static final String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME = "setStructureClass";

	private static final String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME, StructureClassStruct.class);

	@Override
	public void generate(final GeneratorState generatorState) {
		final String structureName = structureSymbol.getName();

		final String systemTimePostfix = "_" + System.nanoTime();

		final String structureClassClassName = STRUCT_CLASSES_PACKAGE + structureName + STRUCTURE_CLASS_POSTFIX + systemTimePostfix;
		final String structureClassClassDesc = 'L' + structureClassClassName + ';';
		final String structureObjectClassName = STRUCT_OBJECTS_PACKAGE + structureName + STRUCTURE_OBJECT_POSTFIX + systemTimePostfix;

		generateStructureObject(generatorState,
		                        structureClassClassName,
		                        structureClassClassDesc,
		                        structureObjectClassName);
		generateStructureClass(generatorState,
		                       structureClassClassName,
		                       structureClassClassDesc,
		                       structureObjectClassName);

		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		if (!classBuilderDeque.isEmpty()) {
			final JavaMethodBuilder previousMethodBuilder = generatorState.getCurrentMethodBuilder();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			final int packageStore = previousMethodBuilder.getNextAvailableStore();
			final int symbolStore = previousMethodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(structureSymbol, generatorState, packageStore, symbolStore);

			previousMv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			previousMv.visitInsn(Opcodes.DUP); // DUP the symbol so it will still be on the stack after we set the structure class.

			previousMv.visitFieldInsn(Opcodes.GETSTATIC, structureClassClassName, GenerationConstants.SINGLETON_INSTANCE, structureClassClassDesc);
			previousMv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                           GenerationConstants.SYMBOL_STRUCT_NAME,
			                           GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME,
			                           GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC,
			                           true);
		}
	}

	/**
	 * Private method for generating a new {@link StructureClassStruct}, by performing the following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Generating the code for the {@code INSTANCE} singleton field</li>
	 * <li>Generating the code for the standard argument constructor</li>
	 * <li>Generating the code for the type addition argument constructor</li>
	 * <li>Generating the code for the {@link StructureClassStruct#newInstance()} method</li>
	 * <li>Generating the code for the class level initialization method ({@code clinit})</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.classes;
	 *
	 * import java.util.List;
	 * import PackageStruct;
	 * import StructureClassStruct;
	 * import StructureObjectStruct;
	 * import jcl.structures.struct.objects.FOOStructureObject_1;
	 * import SymbolStruct;
	 *
	 * public class FOOStructureClass_1 extends StructureClassStruct {
	 *      public static final FOOStructureClass_1 INSTANCE;
	 *
	 *      protected FOOStructureClass_1(SymbolStruct var1, SymbolStruct var2, SymbolStruct var3) {
	 *          super(var1, var2, var3);
	 *      }
	 *
	 *      public StructureObjectStruct newInstance() {
	 *          return new FOOStructureObject_1();
	 *      }
	 *
	 *      static {
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *          PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var4 = var1.intern("MAKE-FOO").getSymbol();
	 *          INSTANCE = new FOOStructureClass_1(var2, var4, (SymbolStruct)null);
	 *          var2.setStructureClass(INSTANCE);
	 *      }
	 * }
	 * }
	 * </pre>
	 * NOTE: It is possible the generated class will not extend {@link StructureClassStruct}, if there is an included
	 * structure in the defstruct operation.
	 * As an example, it will transform {@code (compiler:%defstruct bar foo make-bar nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.classes;
	 *
	 * import java.util.List;
	 * import PackageStruct;
	 * import StructureObjectStruct;
	 * import jcl.structures.struct.classes.FOOStructureClass_1;
	 * import jcl.structures.struct.objects.BARStructureObject_1;
	 * import SymbolStruct;
	 *
	 * public class BARStructureClass_1 extends FOOStructureClass_1 {
	 *      public static final BARStructureClass_1 INSTANCE;
	 *
	 *      protected BARStructureClass_1(SymbolStruct var1, SymbolStruct var2, SymbolStruct var3) {
	 *          super(var1, var2, var3);
	 *      }
	 *
	 *      public StructureObjectStruct newInstance() {
	 *          return new BARStructureObject_1();
	 *      }
	 *
	 *      static {
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("BAR").getSymbol();
	 *          PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var4 = var1.intern("MAKE-BAR").getSymbol();
	 *          INSTANCE = new BARStructureClass_1(var2, var4, (SymbolStruct)null);
	 *          var2.setStructureClass(INSTANCE);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private void generateStructureClass(final GeneratorState generatorState,
	                                           final String structureClassClassName,
	                                           final String structureClassClassDesc,
	                                           final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureClassClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureClassClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final String includeStructureClassFileName;
		if (includeStructureClass == null) {
			includeStructureClassFileName = Type.getInternalName(StructureClassStruct.class);
		} else {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visit(Opcodes.V15, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         structureClassClassName,
		         null,
		         includeStructureClassFileName,
		         null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		CodeGenerators.generateSingletonInstanceField(cw, structureClassClassDesc);
		generateStructureClassConstructor(generatorState, cw,
		                                  includeStructureClassFileName);
		generateStructureClassNewInstanceMethod(generatorState, cw,
		                                        structureObjectClassName);
		generateStructureClassClassInitMethod(generatorState, cw,
		                                      structureClassClassName, structureClassClassDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct#StructureClassStruct(SymbolStruct, SymbolStruct,
	 * SymbolStruct)} constructor for the generated {@link StructureClassStruct} being written to via the
	 * provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the super class {@link StructureClassStruct#StructureClassStruct(SymbolStruct,
	 * SymbolStruct, SymbolStruct)}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * protected FOOStructureClass_1(SymbolStruct var1, SymbolStruct var2, SymbolStruct var3) {
	 *      super(var1, var2, var3);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param includeStructureClassFileName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureClassStruct} super class, whether {@link StructureClassStruct} itself or another subclass {@link
	 * 		StructureClassStruct} implementation
	 */
	private static void generateStructureClassConstructor(final GeneratorState generatorState,
	                                                             final ClassWriter cw,
	                                                             final String includeStructureClassFileName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        STRUCTURE_CLASS_INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int typeArgStore = methodBuilder.getNextAvailableStore();
		final int defaultConstructorSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int printerSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int directSuperClassesArgStore = methodBuilder.getNextAvailableStore();
		final int subClassesArgStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, typeArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   includeStructureClassFileName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_CLASS_INIT_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct#newInstance()} method for the generated {@link
	 * StructureClassStruct} being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Generating the code to create and return a new instance of the {@link StructureObjectStruct} with the
	 * provided {@code structureObjectClassName} class name</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public StructureObjectStruct newInstance() {
	 *      return new FOOStructureObject_1();
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to be created and returned from
	 * 		the {@link StructureClassStruct#newInstance()} method
	 */
	private static void generateStructureClassNewInstanceMethod(final GeneratorState generatorState,
	                                                            final ClassWriter cw,
	                                                            final String structureObjectClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME,
		                                        STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureObjectClassName);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureObjectClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.INIT_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct} class level initialization method ({@code
	 * clinit}) for the generated {@link StructureClassStruct} being written to via the provided {@link ClassWriter}.
	 * The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to fetch the {@link SymbolStruct} for the {@link DefstructStruct#defaultConstructorSymbol}
	 * if it is not {@code null}, or generating {@code null}</li>
	 * <li>Generating the code to fetch the {@link SymbolStruct} for the {@link DefstructStruct#printerSymbol} if it is
	 * not {@code null}, or generating {@code null}</li>
	 * <li>Generating null for both the 'directSuperClasses' and 'subClasses' {@link List}s</li>
	 * <li>Generating the code to create a new instance of the {@link StructureClassStruct} with the provided {@code
	 * structureClassClassName} and storing it into the singleton {@code INSTANCE} field</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * static {
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *      PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var4 = var1.intern("MAKE-FOO").getSymbol();
	 *      INSTANCE = new FOOStructureClass_1(var2, var4, (SymbolStruct)null);
	 *      var2.setStructureClass(INSTANCE);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this class initialization
	 * 		method
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this class
	 * 		initialization method
	 */
	private void generateStructureClassClassInitMethod(final GeneratorState generatorState,
	                                                          final ClassWriter cw,
	                                                          final String structureClassClassName,
	                                                          final String structureClassClassDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
		                                        GenerationConstants.CLASS_INIT_METHOD_NAME,
		                                        GenerationConstants.CLASS_INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureClassClassName);
		mv.visitInsn(Opcodes.DUP);

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(structureSymbol, generatorState, namePackageStore, nameSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);

		if (defaultConstructorSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(defaultConstructorSymbol, generatorState, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		if (printerSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(printerSymbol, generatorState, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureClassClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_CLASS_INIT_METHOD_DESC,
		                   false);
		mv.visitFieldInsn(Opcodes.PUTSTATIC,
		                  structureClassClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureClassClassDesc);

		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  structureClassClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureClassClassDesc);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.SYMBOL_STRUCT_NAME,
		                   SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME,
		                   SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC,
		                   true);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating a new {@link StructureObjectStruct}, by performing the following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Generating the code for the no-argument constructor</li>
	 * <li>Generating the code for the {@code initSlots} method</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.objects;
	 *
	 * import java.util.Map;
	 * import PackageStruct;
	 * import StructureObjectStructImpl;
	 * import jcl.structures.struct.classes.FOOStructureClass_1;
	 * import SymbolStruct;
	 *
	 * public class FOOStructureObject_1 extends StructureObjectStructImpl {
	 *
	 *      public FOOStructureObject_1() {
	 *          FOOStructureClass_1 var10001 = FOOStructureClass_1.INSTANCE;
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *          super(var10001, var2, (StructureObjectStruct)null);
	 *          this.initSlotsMap();
	 *      }
	 *
	 *      private void initSlotsMap() {
	 *          Map var1 = this.slots;
	 *          PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var3 = var2.intern("A").getSymbol();
	 *          var1.put(var3, NILStruct#INSTANCE);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private void generateStructureObject(final GeneratorState generatorState,
	                                     final String structureClassClassName,
	                                     final String structureClassClassDesc,
	                                     final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureObjectClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureObjectClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V15, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         structureObjectClassName,
		         null,
		         GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
		         null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateStructureObjectConstructor(generatorState, cw,
		                                   structureClassClassName, structureClassClassDesc, structureObjectClassName);
		generateStructureObjectInitSlotsMap(generatorState, cw,
		                                    structureObjectClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the constructor for the generated {@link StructureObjectStruct} being written to
	 * via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to grab the static singleton {@code INSTANCE} field from the associated {@link
	 * StructureClassStruct}</li>
	 * <li>Generating the code to retrieve the {@link SymbolStruct} identifying the {@link StructureObjectStruct}</li>
	 * <li>If the provided {@link DefstructStruct#includeStructureClass} is {@code null}, generating {@code null} to be
	 * used as the parent {@link StructureObjectStruct} instance</li>
	 * <li>If the provided {@link DefstructStruct#includeStructureClass} is not {@code null}, generating the code to
	 * grab the static singleton {@code INSTANCE} field from the associated {@link StructureClassStruct} of the parent
	 * {@link StructureObjectStruct} and invoking {@link StructureClassStruct#newInstance()} to retrieve and use a new
	 * instance of the associated parent {@link StructureObjectStruct}</li>
	 * <li>Optionally generating the code to invoke the initialization of the {@link StructureObjectStruct#getSlots()} map
	 * depending on whether or not the {@link DefstructStruct#slots} list is empty or not</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered (no included parent, with slots):
	 * <pre>
	 * {@code
	 * public FOOStructureObject_1() {
	 *      FOOStructureClass_1 var10001 = FOOStructureClass_1.INSTANCE;
	 *
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *
	 *      super(var10001, var2, (StructureObjectStruct)null);
	 *      this.initSlotsMap();
	 * }
	 * }
	 * </pre>
	 * The following is the example Java code generated when {@code (compiler:%defstruct bar foo make-bar nil)} is
	 * encountered (included parent, without slots):
	 * <pre>
	 * {@code
	 * public BARStructureObject_1() {
	 *      BARStructureClass_1 var10001 = BARStructureClass_1.INSTANCE;
	 *
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("BAR").getSymbol();
	 *
	 *      super(var10001, var2, FOOStructureClass_1.INSTANCE.newInstance());
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private void generateStructureObjectConstructor(final GeneratorState generatorState,
	                                                       final ClassWriter cw,
	                                                       final String structureClassClassName,
	                                                       final String structureClassClassDesc,
	                                                       final String structureObjectClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  structureClassClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureClassClassDesc);

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(structureSymbol, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		if (includeStructureClass == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final String includeStructureClassClassName = Type.getInternalName(includeStructureClass.getClass());
			final String includeStructureClassClassDesc = 'L' + includeStructureClassClassName + ';';
			mv.visitFieldInsn(Opcodes.GETSTATIC,
			                  includeStructureClassClassName,
			                  GenerationConstants.SINGLETON_INSTANCE,
			                  includeStructureClassClassDesc);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   includeStructureClassClassName,
			                   STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME,
			                   STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC,
			                   false);
		}
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_OBJECT_INIT_SCS_SS_SOS_METHOD_DESC,
		                   false);

		if (!slots.isEmpty()) {
			// No need to call this method, as there are no slots to initialize
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
			                   structureObjectClassName,
			                   INIT_SLOTS_MAP_METHOD_NAME,
			                   INIT_SLOTS_MAP_METHOD_DESC,
			                   false);
		}

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@code initSlotsMap} initializing method for the generated {@link
	 * StructureObjectStruct} being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * DefstructStruct#slots} is empty</li>
	 * <li>Generating the code to retrieve the {@link StructureObjectStruct#getSlots()} field</li>
	 * <li>Generating the code to retrieve the slot {@link SymbolStruct} and store {@link NILStruct#INSTANCE} value associated with
	 * that symbol into the {@link StructureObjectStruct#getSlots()} {@link Map}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * protected void initSlotsMap() {
	 *      Map var1 = this.slots;
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("A").getSymbol();
	 *      var1.put(var3, NILStruct#INSTANCE);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the {@code
	 * 		initSlotsMap} method code for
	 */
	private void generateStructureObjectInitSlotsMap(final GeneratorState generatorState,
	                                                 final ClassWriter cw,
	                                                 final String structureObjectClassName) {
		if (slots.isEmpty()) {
			// No need to generate this method, as there are no slots to initialize
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
		                                        INIT_SLOTS_MAP_METHOD_NAME,
		                                        INIT_SLOTS_MAP_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETFIELD, structureObjectClassName, SLOTS_FIELD, GenerationConstants.JAVA_MAP_DESC);
		final int slotsFieldStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, slotsFieldStore);

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int slotStore = methodBuilder.getNextAvailableStore();

		for (final SymbolStruct slot : slots) {
			CodeGenerators.generateSymbol(slot, generatorState, packageStore, slotStore);

			mv.visitVarInsn(Opcodes.ALOAD, slotsFieldStore);
			mv.visitVarInsn(Opcodes.ALOAD, slotStore);
			NILStruct.INSTANCE.generate(generatorState);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_MAP_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}
}
