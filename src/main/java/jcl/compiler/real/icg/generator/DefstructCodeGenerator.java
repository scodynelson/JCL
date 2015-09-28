package jcl.compiler.real.icg.generator;

import java.util.Deque;
import java.util.List;

import jcl.LispType;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.defstruct.DefstructStruct;
import jcl.structures.StructureClassStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StructureObjectType;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
class DefstructCodeGenerator implements CodeGenerator<DefstructStruct> {

	private static final String STRUCT_TYPES_PACKAGE = "jcl/structures/struct/types/";

	private static final String STRUCT_CLASSES_PACKAGE = "jcl/structures/struct/classes/";

	private static final String STRUCT_OBJECTS_PACKAGE = "jcl/structures/struct/objects/";

	private static final String STRUCTURE_TYPE_POSTFIX = "StructureType";

	private static final String STRUCTURE_CLASS_POSTFIX = "StructureClass";

	private static final String STRUCTURE_OBJECT_POSTFIX = "StructureObject";

	private static final String SYNTHETIC_INNER_CLASS_ID = "$1";

	private static final String SYNTHETIC_INNER_CLASS_DESC_PREFIX = "(L";

	private static final String SYNTHETIC_INNER_CLASS_DESC_POSTFIX = ";)V";

	private static final String STRUCTURE_TYPE_FACTORY_POSTFIX = "Factory";

	private static final String STRUCTURE_TYPE_IMPL_POSTFIX = "StructureTypeImpl";

	private static final String INIT_SLOTS_MAP_METHOD_NAME = "initSlotsMap";

	private static final String INIT_SLOTS_MAP_METHOD_DESC = "()V";

	private static final String SLOTS_FIELD = "slots";

	private static final String STRUCTURE_TYPE_FACTORY_SIGNATURE_PREFIX = "Ljava/lang/Object;Ljcl/types/TypeFactory<L";

	private static final String STRUCTURE_TYPE_FACTORY_SIGNATURE_POSTFIX = ";>;";

	private static final String[] STRUCTURE_TYPE_FACTORY_INTERFACES = {GenerationConstants.TYPE_FACTORY_NAME};

	private static final String GET_INSTANCE_METHOD_NAME = "getInstance";

	private static final String GET_INSTANCE_METHOD_DESC = "()Ljcl/LispType;";

	private static final String STRUCTURE_OBJECT_INIT_SCS_SS_SOS_METHOD_DESC = "(Ljcl/structures/StructureClassStruct;Ljcl/symbols/SymbolStruct;Ljcl/structures/StructureObjectStruct;)V";

	private static final String STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC = "(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V";

	private static final String STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_SIGNATURE = "(Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V";

	private static final String STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC = "(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V";

	private static final String STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_SIGNATURE = "(Ljcl/LispType;Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME = "newInstance";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC = "()Ljcl/structures/StructureObjectStruct;";

	@Override
	public void generate(final DefstructStruct input, final GeneratorState generatorState) {

		final SymbolStruct<?> structureSymbol = input.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String systemTimePostfix = "_" + System.nanoTime();

		final String structureTypeClassName = STRUCT_TYPES_PACKAGE + structureName + STRUCTURE_TYPE_POSTFIX + systemTimePostfix;
		final String structureTypeClassDesc = 'L' + structureTypeClassName + ';';
		final String structureClassClassName = STRUCT_CLASSES_PACKAGE + structureName + STRUCTURE_CLASS_POSTFIX + systemTimePostfix;
		final String structureClassClassDesc = 'L' + structureClassClassName + ';';
		final String structureObjectClassName = STRUCT_OBJECTS_PACKAGE + structureName + STRUCTURE_OBJECT_POSTFIX + systemTimePostfix;

		final String structureTypeSyntheticInnerClassName = structureTypeClassName + SYNTHETIC_INNER_CLASS_ID + systemTimePostfix;
		final String structureTypeSyntheticInnerClassDesc = SYNTHETIC_INNER_CLASS_DESC_PREFIX + structureTypeSyntheticInnerClassName + SYNTHETIC_INNER_CLASS_DESC_POSTFIX;
		final String structureTypeFactoryInnerClassName = structureTypeClassName + '$' + STRUCTURE_TYPE_FACTORY_POSTFIX + systemTimePostfix;

		final String structureTypeImplClassName = structureName + STRUCTURE_TYPE_IMPL_POSTFIX + systemTimePostfix;
		final String structureTypeImplInnerClassName = structureTypeFactoryInnerClassName + '$' + structureTypeImplClassName;

		generateStructureObject(input, generatorState,
				structureClassClassName,
				structureClassClassDesc,
				structureObjectClassName);
		generateStructureClass(input, generatorState,
				structureTypeClassName,
				structureTypeClassDesc,
				structureClassClassName,
				structureClassClassDesc,
				structureObjectClassName);
		generateStructureTypeImpl(generatorState,
				structureName,
				structureTypeClassName,
				structureTypeSyntheticInnerClassName,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				structureTypeImplInnerClassName);
		generateStructureTypeFactory(generatorState,
				structureTypeClassName,
				structureTypeClassDesc,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				structureTypeImplInnerClassName);
		generateStructureType(input, generatorState,
				structureTypeClassName,
				structureTypeClassDesc,
				structureTypeSyntheticInnerClassName,
				structureTypeSyntheticInnerClassDesc,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				structureTypeImplInnerClassName);

		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		if (!classBuilderDeque.isEmpty()) {
			final JavaMethodBuilder previousMethodBuilder = generatorState.getCurrentMethodBuilder();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			final int packageStore = previousMethodBuilder.getNextAvailableStore();
			final int symbolStore = previousMethodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(structureSymbol, previousMethodBuilder, packageStore, symbolStore);

			previousMv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			previousMv.visitInsn(Opcodes.DUP); // DUP the symbol so it will still be on the stack after we set the structure class.

			previousMv.visitFieldInsn(Opcodes.GETSTATIC, structureClassClassName, GenerationConstants.SINGLETON_INSTANCE, structureClassClassDesc);
			previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC,
					false);
		}
	}

	private static void generateStructureType(final DefstructStruct input, final GeneratorState generatorState,
	                                          final String structureTypeClassName,
	                                          final String structureTypeClassDesc,
	                                          final String structureTypeSyntheticInnerClassName,
	                                          final String structureTypeSyntheticInnerClassDesc,
	                                          final String structureTypeFactoryInnerClassName,
	                                          final String structureTypeImplClassName,
	                                          final String structureTypeImplInnerClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();

		final String[] interfaces = new String[1];
		if (includeStructureClass == null) {
			interfaces[0] = Type.getInternalName(StructureObjectType.class);
		} else {
			final LispType includeStructureClassType = includeStructureClass.getType();
			final Class<?> includeStructureClassTypeClass = includeStructureClassType.getClass();
			final Class<?>[] includeStructureClassTypeInterfaces = includeStructureClassTypeClass.getInterfaces();
			if (includeStructureClassTypeInterfaces.length == 0) {
				interfaces[0] = Type.getInternalName(StructureObjectType.class);
			} else {
				// NOTE: Because of the way we build these structure types, the first interface will be the one we want.
				final Class<?> includeStructureClassTypeInterface = includeStructureClassTypeInterfaces[0];
				final String includeStructureClassTypeName = Type.getInternalName(includeStructureClassTypeInterface);

				interfaces[0] = includeStructureClassTypeName;
			}
		}

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE,
				structureTypeClassName,
				null,
				GenerationConstants.JAVA_OBJECT_NAME,
				interfaces);

		cw.visitInnerClass(structureTypeSyntheticInnerClassName,
				null,
				null,
				Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);
		cw.visitInnerClass(structureTypeFactoryInnerClassName,
				structureTypeClassName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClassName,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateStructureInstanceField(cw, structureTypeClassDesc);
		generateStructureTypeClassInitMethod(generatorState, cw,
				structureTypeClassName, structureTypeClassDesc,
				structureTypeSyntheticInnerClassDesc, structureTypeImplInnerClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeClassInitMethod(final GeneratorState generatorState, final ClassWriter cw,
	                                                         final String structureTypeClassName,
	                                                         final String structureTypeClassDesc,
	                                                         final String structureTypeSyntheticInnerClassDesc,
	                                                         final String structureTypeImplInnerClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
				GenerationConstants.CLASS_INIT_METHOD_NAME,
				GenerationConstants.CLASS_INIT_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureTypeImplInnerClassName);
		mv.visitInsn(Opcodes.DUP);
		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				structureTypeImplInnerClassName,
				GenerationConstants.INIT_METHOD_NAME,
				structureTypeSyntheticInnerClassDesc,
				false);
		mv.visitFieldInsn(Opcodes.PUTSTATIC,
				structureTypeClassName,
				GenerationConstants.SINGLETON_INSTANCE,
				structureTypeClassDesc);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeFactory(final GeneratorState generatorState,
	                                                 final String structureTypeClassName,
	                                                 final String structureTypeClassDesc,
	                                                 final String structureTypeFactoryInnerClassName,
	                                                 final String structureTypeImplClassName,
	                                                 final String structureTypeImplInnerClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeFactoryInnerClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeFactoryInnerClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureTypeFactoryInnerClassName,
				STRUCTURE_TYPE_FACTORY_SIGNATURE_PREFIX + structureTypeClassName + STRUCTURE_TYPE_FACTORY_SIGNATURE_POSTFIX,
				GenerationConstants.JAVA_OBJECT_NAME,
				STRUCTURE_TYPE_FACTORY_INTERFACES);

		cw.visitInnerClass(structureTypeFactoryInnerClassName,
				structureTypeClassName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClassName,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		final String structureTypeGetInstanceDesc = "()L" + structureTypeClassName + ';';

		generateStructureTypeFactoryConstructor(generatorState, cw);
		generateStructureTypeFactoryGetInstanceMethod(generatorState, cw,
				structureTypeClassName, structureTypeClassDesc, structureTypeGetInstanceDesc);
		generateStructureTypeFactorySyntheticBridgeGetInstanceMethod(generatorState, cw,
				structureTypeFactoryInnerClassName, structureTypeGetInstanceDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeFactoryConstructor(final GeneratorState generatorState,
	                                                            final ClassWriter cw) {
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
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.INIT_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeFactoryGetInstanceMethod(final GeneratorState generatorState,
	                                                                  final ClassWriter cw,
	                                                                  final String structureTypeClassName,
	                                                                  final String structureTypeClassDesc,
	                                                                  final String structureTypeGetInstanceDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				GET_INSTANCE_METHOD_NAME,
				structureTypeGetInstanceDesc,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
				structureTypeClassName,
				GenerationConstants.SINGLETON_INSTANCE,
				structureTypeClassDesc);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeFactorySyntheticBridgeGetInstanceMethod(final GeneratorState generatorState,
	                                                                                 final ClassWriter cw,
	                                                                                 final String structureTypeFactoryInnerClassName,
	                                                                                 final String structureTypeGetInstanceDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC,
				GET_INSTANCE_METHOD_NAME,
				GET_INSTANCE_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				structureTypeFactoryInnerClassName,
				GET_INSTANCE_METHOD_NAME,
				structureTypeGetInstanceDesc,
				false);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeImpl(final GeneratorState generatorState,
	                                              final String structureName,
	                                              final String structureTypeClassName,
	                                              final String structureTypeSyntheticInnerClassName,
	                                              final String structureTypeFactoryInnerClassName,
	                                              final String structureTypeImplClassName,
	                                              final String structureTypeImplInnerClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeImplInnerClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeImplInnerClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
				structureTypeImplInnerClassName,
				null,
				GenerationConstants.TYPE_BASE_CLASS_NAME,
				new String[]{structureTypeClassName, GenerationConstants.ATOMIC_TYPE_SPECIFIER_NAME});

		cw.visitInnerClass(structureTypeFactoryInnerClassName,
				structureTypeClassName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClassName,
				structureTypeFactoryInnerClassName,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeSyntheticInnerClassName,
				null,
				null,
				Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		CodeGenerators.generateSerialVersionUIDField(cw);
		generateStructureTypeImplConstructor(generatorState, cw,
				structureName);
		generateStructureTypeImplHashCodeMethod(generatorState, cw);
		generateStructureTypeImplEqualsMethod(generatorState, cw,
				structureTypeClassName);
		generateStructureTypeSyntheticConstructor(generatorState, cw,
				structureTypeSyntheticInnerClassName, structureTypeImplInnerClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeImplConstructor(final GeneratorState generatorState,
	                                                         final ClassWriter cw,
	                                                         final String structureName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
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
		mv.visitLdcInsn(structureName);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.TYPE_BASE_CLASS_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.TYPE_BASE_CLASS_INIT_STRING_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeImplHashCodeMethod(final GeneratorState generatorState,
	                                                            final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				GenerationConstants.JAVA_HASH_CODE_METHOD_NAME,
				GenerationConstants.JAVA_HASH_CODE_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.HASH_CODE_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.HASH_CODE_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.INIT_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.TYPE_BASE_CLASS_NAME,
				GenerationConstants.JAVA_HASH_CODE_METHOD_NAME,
				GenerationConstants.JAVA_HASH_CODE_METHOD_DESC,
				false);

		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.HASH_CODE_BUILDER_NAME,
				GenerationConstants.HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME,
				GenerationConstants.HASH_CODE_BUILDER_APPEND_SUPER_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.HASH_CODE_BUILDER_NAME,
				GenerationConstants.HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME,
				GenerationConstants.HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.IRETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeImplEqualsMethod(final GeneratorState generatorState,
	                                                          final ClassWriter cw,
	                                                          final String structureTypeClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int objStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, objStore);

		final Label thisEqObject = new Label();
		final Label objNotInstanceOfType = new Label();
		final Label endOfEqAndInstanceCheck = new Label();

		mv.visitJumpInsn(Opcodes.IF_ACMPEQ, thisEqObject);

		mv.visitVarInsn(Opcodes.ALOAD, objStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, structureTypeClassName);

		mv.visitJumpInsn(Opcodes.IFEQ, objNotInstanceOfType);

		mv.visitLabel(thisEqObject);
		mv.visitInsn(Opcodes.ICONST_1);
		mv.visitJumpInsn(Opcodes.GOTO, endOfEqAndInstanceCheck);

		mv.visitLabel(objNotInstanceOfType);
		mv.visitInsn(Opcodes.ICONST_0);

		mv.visitLabel(endOfEqAndInstanceCheck);

		mv.visitInsn(Opcodes.IRETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureTypeSyntheticConstructor(final GeneratorState generatorState,
	                                                              final ClassWriter cw,
	                                                              final String structureTypeSyntheticInnerClassDesc,
	                                                              final String structureTypeImplInnerClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_SYNTHETIC,
				GenerationConstants.INIT_METHOD_NAME,
				structureTypeSyntheticInnerClassDesc,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				structureTypeImplInnerClassName,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.INIT_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureClass(final DefstructStruct input, final GeneratorState generatorState,
	                                           final String structureTypeClassName,
	                                           final String structureTypeClassDesc,
	                                           final String structureClassClassName,
	                                           final String structureClassClassDesc,
	                                           final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureClassClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureClassClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();
		final String includeStructureClassFileName;
		if (includeStructureClass == null) {
			includeStructureClassFileName = Type.getInternalName(StructureClassStruct.class);
		} else {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureClassClassName,
				null,
				includeStructureClassFileName,
				null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateStructureInstanceField(cw, structureClassClassDesc);
		CodeGenerators.generateSerialVersionUIDField(cw);
		generateStructureClassConstructor(generatorState, cw,
				structureTypeClassName, structureTypeClassDesc, structureClassClassName);
		generateStructureClassTypeArgConstructor(generatorState, cw,
				includeStructureClassFileName);
		generateStructureClassNewInstanceMethod(generatorState, cw,
				structureObjectClassName);
		generateStructureClassClassInitMethod(input, generatorState, cw,
				structureClassClassName, structureClassClassDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	private static void generateStructureInstanceField(final ClassWriter cw, final String structureClassClassDesc) {
		final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
				GenerationConstants.SINGLETON_INSTANCE,
				structureClassClassDesc,
				null,
				null);

		fv.visitEnd();
	}

	private static void generateStructureClassConstructor(final GeneratorState generatorState,
	                                                      final ClassWriter cw,
	                                                      final String structureTypeClassName,
	                                                      final String structureTypeClassDesc,
	                                                      final String structureClassClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GenerationConstants.INIT_METHOD_NAME,
				STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC,
				STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int defaultConstructorSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int printerSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int directSuperClassesArgStore = methodBuilder.getNextAvailableStore();
		final int subClassesArgStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
				structureTypeClassName,
				GenerationConstants.SINGLETON_INSTANCE,
				structureTypeClassDesc);
		mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				structureClassClassName,
				GenerationConstants.INIT_METHOD_NAME,
				STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureClassTypeArgConstructor(final GeneratorState generatorState,
	                                                             final ClassWriter cw,
	                                                             final String includeStructureClassFileName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GenerationConstants.INIT_METHOD_NAME,
				STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
				STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_SIGNATURE,
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
				STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

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

	private static void generateStructureClassClassInitMethod(final DefstructStruct input, final GeneratorState generatorState,
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
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureClassClassName);
		mv.visitInsn(Opcodes.DUP);

		final SymbolStruct<?> defaultConstructorSymbol = input.getDefaultConstructorSymbol();
		if (defaultConstructorSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(defaultConstructorSymbol, methodBuilder, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		final SymbolStruct<?> printerSymbol = input.getPrinterSymbol();
		if (printerSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(printerSymbol, methodBuilder, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitInsn(Opcodes.ACONST_NULL);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				structureClassClassName,
				GenerationConstants.INIT_METHOD_NAME,
				STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC,
				false);
		mv.visitFieldInsn(Opcodes.PUTSTATIC,
				structureClassClassName,
				GenerationConstants.SINGLETON_INSTANCE,
				structureClassClassDesc);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureObject(final DefstructStruct input, final GeneratorState generatorState,
	                                            final String structureClassClassName,
	                                            final String structureClassClassDesc,
	                                            final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureObjectClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureObjectClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureObjectClassName,
				null,
				GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
				null);

		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();
		String includeStructureClassClassName = null;
		if (includeStructureClass != null) {
			includeStructureClassClassName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		CodeGenerators.generateSerialVersionUIDField(cw);
		generateStructureObjectNoArgConstructor(input, generatorState, cw,
				structureClassClassName, structureClassClassDesc,
				structureObjectClassName, includeStructureClassClassName);
		generateStructureObjectInitSlotsMap(input, generatorState, cw,
				structureObjectClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	private static void generateStructureObjectNoArgConstructor(final DefstructStruct input, final GeneratorState generatorState,
	                                                            final ClassWriter cw,
	                                                            final String structureClassClassName,
	                                                            final String structureClassClassDesc,
	                                                            final String structureObjectClassName,
	                                                            final String includeStructureClassClassName) {
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

		final SymbolStruct<?> structureSymbol = input.getStructureSymbol();
		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(structureSymbol, methodBuilder, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		if (includeStructureClassClassName == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
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

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				structureObjectClassName,
				INIT_SLOTS_MAP_METHOD_NAME,
				INIT_SLOTS_MAP_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateStructureObjectInitSlotsMap(final DefstructStruct input, final GeneratorState generatorState,
	                                                        final ClassWriter cw,
	                                                        final String structureObjectClassName) {
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

		final List<SymbolStruct<?>> slots = input.getSlots();
		for (final SymbolStruct<?> slot : slots) {
			CodeGenerators.generateSymbol(slot, methodBuilder, packageStore, slotStore);

			mv.visitVarInsn(Opcodes.ALOAD, slotsFieldStore);
			mv.visitVarInsn(Opcodes.ALOAD, slotStore);
			mv.visitInsn(Opcodes.ACONST_NULL);
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
