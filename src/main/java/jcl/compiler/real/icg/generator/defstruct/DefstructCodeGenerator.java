package jcl.compiler.real.icg.generator.defstruct;

import java.security.SecureRandom;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import jcl.LispType;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.simple.SymbolCodeGeneratorUtil;
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
public class DefstructCodeGenerator implements CodeGenerator<DefstructStruct> {

	private static final String STRUCT_TYPES_PACKAGE = "jcl/structures/struct/types/";

	private static final String STRUCT_CLASSES_PACKAGE = "jcl/structures/struct/classes/";

	private static final String STRUCT_OBJECTS_PACKAGE = "jcl/structures/struct/objects/";

	private static final String STRUCTURE_TYPE_POSTFIX = "StructureType";

	private static final String STRUCTURE_CLASS_POSTFIX = "StructureClass";

	private static final String STRUCTURE_OBJECT_POSTFIX = "StructureObject";

	private static final String SYNTHETIC_INNER_CLASS_ID = "$1";

	private static final String STRUCTURE_TYPE_FACTORY_POSTFIX = "Factory";

	private static final String STRUCTURE_TYPE_IMPL_POSTFIX = "StructureTypeImpl";

	private static final String INSTANCE_FIELD = "INSTANCE";

	public static final String INIT_SLOTS_MAP_METHOD_NAME = "initSlotsMap";

	public static final String SLOTS_FIELD = "slots";

	@Override
	public void generate(final DefstructStruct input, final JavaClassBuilder classBuilder) {

		final SymbolStruct<?> structureSymbol = input.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String systemTimePostfix = "_" + System.nanoTime();

		final String structureTypeFileName = STRUCT_TYPES_PACKAGE + structureName + STRUCTURE_TYPE_POSTFIX + systemTimePostfix;
		final String structureClassFileName = STRUCT_CLASSES_PACKAGE + structureName + STRUCTURE_CLASS_POSTFIX + systemTimePostfix;
		final String structureObjectFileName = STRUCT_OBJECTS_PACKAGE + structureName + STRUCTURE_OBJECT_POSTFIX + systemTimePostfix;

		final String structureTypeSyntheticInnerClass = structureTypeFileName + SYNTHETIC_INNER_CLASS_ID + systemTimePostfix;
		final String structureTypeFactoryInnerClass = structureTypeFileName + '$' + STRUCTURE_TYPE_FACTORY_POSTFIX + systemTimePostfix;

		final String structureTypeImplClassName = structureName + STRUCTURE_TYPE_IMPL_POSTFIX + systemTimePostfix;
		final String structureTypeImplInnerClass = structureTypeFactoryInnerClass + '$' + structureTypeImplClassName;

		generateStructureObject(input, classBuilder,
				structureClassFileName,
				structureObjectFileName);
		generateStructureClass(input, classBuilder,
				structureTypeFileName,
				structureClassFileName,
				structureObjectFileName);
		generateStructureTypeImpl(classBuilder,
				structureName,
				structureTypeFileName,
				structureTypeSyntheticInnerClass,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				structureTypeImplInnerClass);
		generateStructureTypeFactory(classBuilder,
				structureTypeFileName,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				structureTypeImplInnerClass);
		generateStructureType(input, classBuilder,
				structureTypeFileName,
				structureTypeSyntheticInnerClass,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				structureTypeImplInnerClass);

		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);

			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			final int packageStore = previousMethodBuilder.getNextAvailableStore();
			final int symbolStore = previousMethodBuilder.getNextAvailableStore();
			SymbolCodeGeneratorUtil.generate(structureSymbol, classBuilder, packageStore, symbolStore);

			previousMv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			previousMv.visitInsn(Opcodes.DUP); // DUP the symbol so it will still be on the stack after we set the structure class.

			previousMv.visitFieldInsn(Opcodes.GETSTATIC, structureClassFileName, INSTANCE_FIELD, 'L' + structureClassFileName + ';');
			previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC,
					false);
		}
	}

	public static void generateStructureType(final DefstructStruct defstructStruct, final JavaClassBuilder classBuilder,
	                                         final String structureTypeFileName,
	                                         final String structureTypeSyntheticInnerClass,
	                                         final String structureTypeFactoryInnerClass,
	                                         final String structureTypeImplClassName,
	                                         final String structureTypeImplInnerClass) {

		final String className = structureTypeFileName.substring(structureTypeFileName.lastIndexOf('/') + 1, structureTypeFileName.length());

		final ClassDef currentClass = new ClassDef(structureTypeFileName, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final StructureClassStruct includeStructureClass = defstructStruct.getIncludeStructureClass();

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
				structureTypeFileName,
				null,
				GenerationConstants.JAVA_OBJECT_NAME,
				interfaces);

		cw.visitInnerClass(structureTypeSyntheticInnerClass,
				null,
				null,
				Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);
		cw.visitInnerClass(structureTypeFactoryInnerClass,
				structureTypeFileName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					INSTANCE_FIELD,
					'L' + structureTypeFileName + ';',
					null,
					null);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
					GenerationConstants.CLASS_INIT_METHOD_NAME,
					GenerationConstants.CLASS_INIT_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitTypeInsn(Opcodes.NEW, structureTypeImplInnerClass);
			mv.visitInsn(Opcodes.DUP);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureTypeImplInnerClass,
					GenerationConstants.INIT_METHOD_NAME,
					getStructureTypeConstructorDescriptor(structureTypeSyntheticInnerClass),
					false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, structureTypeFileName, INSTANCE_FIELD, 'L' + structureTypeFileName + ';');

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}

		cw.visitEnd();

		classStack.pop();
	}

	public static void generateStructureTypeFactory(final JavaClassBuilder classBuilder,
	                                                final String structureTypeFileName,
	                                                final String structureTypeFactoryInnerClass,
	                                                final String structureTypeImplClassName,
	                                                final String structureTypeImplInnerClass) {

		final String className = structureTypeFactoryInnerClass.substring(structureTypeFactoryInnerClass.lastIndexOf('/') + 1, structureTypeFactoryInnerClass.length());

		final ClassDef currentClass = new ClassDef(structureTypeFactoryInnerClass, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureTypeFactoryInnerClass,
				"Ljava/lang/Object;Ljcl/types/TypeFactory<L" + structureTypeFileName + ";>;",
				GenerationConstants.JAVA_OBJECT_NAME,
				new String[]{"jcl/types/TypeFactory"});

		cw.visitInnerClass(structureTypeFactoryInnerClass,
				structureTypeFileName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.JAVA_OBJECT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					"getInstance",
					"()L" + structureTypeFileName + ';',
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitFieldInsn(Opcodes.GETSTATIC, structureTypeFileName, INSTANCE_FIELD, 'L' + structureTypeFileName + ';');

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC,
					"getInstance",
					"()Ljcl/LispType;",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					structureTypeFactoryInnerClass,
					"getInstance",
					"()L" + structureTypeFileName + ';',
					false);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}

		cw.visitEnd();

		classStack.pop();
	}

	public static void generateStructureTypeImpl(final JavaClassBuilder classBuilder,
	                                             final String structureName,
	                                             final String structureTypeFileName,
	                                             final String structureTypeSyntheticInnerClass,
	                                             final String structureTypeFactoryInnerClass,
	                                             final String structureTypeImplClassName,
	                                             final String structureTypeImplInnerClass) {

		final String className = structureTypeImplInnerClass.substring(structureTypeImplInnerClass.lastIndexOf('/') + 1, structureTypeImplInnerClass.length());

		final ClassDef currentClass = new ClassDef(structureTypeImplInnerClass, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
				structureTypeImplInnerClass,
				null,
				GenerationConstants.TYPE_BASE_CLASS_NAME,
				new String[]{structureTypeFileName, GenerationConstants.ATOMIC_TYPE_SPECIFIER_NAME});

		cw.visitInnerClass(structureTypeFactoryInnerClass,
				structureTypeFileName,
				STRUCTURE_TYPE_FACTORY_POSTFIX,
				Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass,
				structureTypeFactoryInnerClass,
				structureTypeImplClassName,
				Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeSyntheticInnerClass,
				null,
				null,
				Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					GenerationConstants.SERIAL_VERSION_UID_FIELD,
					GenerationConstants.JAVA_LONG_TYPE_NAME,
					null,
					serialVersionUID);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

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

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					"hashCode",
					"()I",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitTypeInsn(Opcodes.NEW, "org/apache/commons/lang3/builder/HashCodeBuilder");
			mv.visitInsn(Opcodes.DUP);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					"org/apache/commons/lang3/builder/HashCodeBuilder",
					"<init>",
					"()V",
					false);
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					"jcl/types/TypeBaseClass",
					"hashCode",
					"()I",
					false);

			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					"org/apache/commons/lang3/builder/HashCodeBuilder",
					"appendSuper",
					"(I)Lorg/apache/commons/lang3/builder/HashCodeBuilder;",
					false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					"org/apache/commons/lang3/builder/HashCodeBuilder",
					"toHashCode",
					"()I",
					false);

			mv.visitInsn(Opcodes.IRETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					GenerationConstants.JAVA_EQUALS_METHOD_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

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
			mv.visitTypeInsn(Opcodes.INSTANCEOF, structureTypeFileName);

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

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_SYNTHETIC,
					GenerationConstants.INIT_METHOD_NAME,
					getStructureTypeConstructorDescriptor(structureTypeSyntheticInnerClass),
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureTypeImplInnerClass,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}

		cw.visitEnd();

		classStack.pop();
	}

	public static void generateStructureClass(final DefstructStruct defstructStruct, final JavaClassBuilder classBuilder,
	                                          final String structureTypeFileName,
	                                          final String structureClassFileName,
	                                          final String structureObjectFileName) {

		final String className = structureClassFileName.substring(structureClassFileName.lastIndexOf('/') + 1, structureClassFileName.length());

		final ClassDef currentClass = new ClassDef(structureClassFileName, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final StructureClassStruct includeStructureClass = defstructStruct.getIncludeStructureClass();
		final String includeStructureClassFileName;
		if (includeStructureClass == null) {
			includeStructureClassFileName = Type.getInternalName(StructureClassStruct.class);
		} else {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureClassFileName,
				null,
				includeStructureClassFileName,
				null);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					INSTANCE_FIELD,
					'L' + structureClassFileName + ';',
					null,
					null);

			fv.visitEnd();
		}
		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					GenerationConstants.SERIAL_VERSION_UID_FIELD,
					GenerationConstants.JAVA_LONG_TYPE_NAME,
					null,
					serialVersionUID);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					GenerationConstants.INIT_METHOD_NAME,
					"(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int defaultConstructorSymbolArgStore = methodBuilder.getNextAvailableStore();
			final int printerSymbolArgStore = methodBuilder.getNextAvailableStore();
			final int directSuperClassesArgStore = methodBuilder.getNextAvailableStore();
			final int subClassesArgStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');
			mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureClassFileName,
					GenerationConstants.INIT_METHOD_NAME,
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					GenerationConstants.INIT_METHOD_NAME,
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

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
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					"newInstance",
					"()Ljcl/structures/StructureObjectStruct;",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitTypeInsn(Opcodes.NEW, structureObjectFileName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureObjectFileName,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					false);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
					GenerationConstants.CLASS_INIT_METHOD_NAME,
					GenerationConstants.CLASS_INIT_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitTypeInsn(Opcodes.NEW, structureClassFileName);
			mv.visitInsn(Opcodes.DUP);

			final SymbolStruct<?> defaultConstructorSymbol = defstructStruct.getDefaultConstructorSymbol();
			if (defaultConstructorSymbol == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				final int packageStore = methodBuilder.getNextAvailableStore();
				final int symbolStore = methodBuilder.getNextAvailableStore();
				SymbolCodeGeneratorUtil.generate(defaultConstructorSymbol, classBuilder, packageStore, symbolStore);

				mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			}

			final SymbolStruct<?> printerSymbol = defstructStruct.getPrinterSymbol();
			if (printerSymbol == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				final int packageStore = methodBuilder.getNextAvailableStore();
				final int symbolStore = methodBuilder.getNextAvailableStore();
				SymbolCodeGeneratorUtil.generate(printerSymbol, classBuilder, packageStore, symbolStore);

				mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			}

			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitInsn(Opcodes.ACONST_NULL);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureClassFileName,
					GenerationConstants.INIT_METHOD_NAME,
					"(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, structureClassFileName, INSTANCE_FIELD, 'L' + structureClassFileName + ';');

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}

		cw.visitEnd();

		classStack.pop();
	}

	public static void generateStructureObject(final DefstructStruct defstructStruct, final JavaClassBuilder classBuilder,
	                                           final String structureClassFileName,
	                                           final String structureObjectFileName) {

		final String className = structureObjectFileName.substring(structureObjectFileName.lastIndexOf('/') + 1, structureObjectFileName.length());

		final ClassDef currentClass = new ClassDef(structureObjectFileName, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				structureObjectFileName,
				null,
				GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
				null);

		final StructureClassStruct includeStructureClass = defstructStruct.getIncludeStructureClass();
		String includeStructureClassFileName = null;
		if (includeStructureClass != null) {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					GenerationConstants.SERIAL_VERSION_UID_FIELD,
					GenerationConstants.JAVA_LONG_TYPE_NAME,
					null,
					serialVersionUID);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					GenerationConstants.INIT_METHOD_NAME,
					"()V",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureClassFileName, INSTANCE_FIELD, 'L' + structureClassFileName + ';');

			final SymbolStruct<?> structureSymbol = defstructStruct.getStructureSymbol();
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			SymbolCodeGeneratorUtil.generate(structureSymbol, classBuilder, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

			if (includeStructureClassFileName == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				mv.visitFieldInsn(Opcodes.GETSTATIC, includeStructureClassFileName, INSTANCE_FIELD, 'L' + includeStructureClassFileName + ';');
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						includeStructureClassFileName,
						"newInstance",
						"()Ljcl/structures/StructureObjectStruct;",
						false);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					"(Ljcl/structures/StructureClassStruct;Ljcl/symbols/SymbolStruct;Ljcl/structures/StructureObjectStruct;)V",
					false);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureObjectFileName,
					INIT_SLOTS_MAP_METHOD_NAME,
					"()V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
					INIT_SLOTS_MAP_METHOD_NAME,
					"()V",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETFIELD, structureObjectFileName, SLOTS_FIELD, GenerationConstants.JAVA_MAP_DESC);
			final int slotsFieldStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, slotsFieldStore);

			final int packageStore = methodBuilder.getNextAvailableStore();
			final int slotStore = methodBuilder.getNextAvailableStore();

			final List<SymbolStruct<?>> slots = defstructStruct.getSlots();
			for (final SymbolStruct<?> slot : slots) {
				SymbolCodeGeneratorUtil.generate(slot, classBuilder, packageStore, slotStore);

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

			methodBuilderStack.pop();
		}

		cw.visitEnd();

		classStack.pop();
	}

	private static String getStructureTypeConstructorDescriptor(final String className) {
		return "(L" + className + ";)V";
	}
}
