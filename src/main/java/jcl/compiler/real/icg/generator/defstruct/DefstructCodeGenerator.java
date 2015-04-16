package jcl.compiler.real.icg.generator.defstruct;

import java.security.SecureRandom;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import jcl.LispType;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
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

	@Override
	public void generate(final DefstructStruct input, final JavaClassBuilder classBuilder) {

		final SymbolStruct<?> structureSymbol = input.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String structureTypeFileName = "jcl/structures/struct/types/" + structureName + "StructureType";
		final String structureClassFileName = "jcl/structures/struct/classes/" + structureName + "StructureClass";
		final String structureObjectFileName = "jcl/structures/struct/objects/" + structureName + "StructureObject";

		final String structureTypeSyntheticInnerClass = structureTypeFileName + "$1";
		final String structureTypeFactoryInnerClass = structureTypeFileName + "$Factory";

		final String structureTypeImplClassName = structureName + "StructureTypeImpl";
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
			final MethodVisitor mv = previousClassDef.getMethodVisitor();

			final String packageName = structureSymbol.getSymbolPackage().getName();
			final String symbolName = structureSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);

			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
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
			final String includeStructureClassTypeName = Type.getInternalName(includeStructureClassType.getClass());

			interfaces[0] = includeStructureClassTypeName;
		}

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE, structureTypeFileName, null, "java/lang/Object", interfaces);

		cw.visitInnerClass(structureTypeSyntheticInnerClass, null, null, Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);
		cw.visitInnerClass(structureTypeFactoryInnerClass, structureTypeFileName, "Factory", Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass, structureTypeFactoryInnerClass, structureTypeImplClassName, Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(className + ".java", null);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "INSTANCE", 'L' + structureTypeFileName + ';', null, null);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, structureTypeImplInnerClass);
			mv.visitInsn(Opcodes.DUP);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureTypeImplInnerClass, "<init>", "(L" + structureTypeSyntheticInnerClass + ";)V", false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
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
				"java/lang/Object",
				new String[]{"jcl/types/TypeFactory"});

		cw.visitInnerClass(structureTypeFactoryInnerClass, structureTypeFileName, "Factory", Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass, structureTypeFactoryInnerClass, structureTypeImplClassName, Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(className + ".java", null);

		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "getInstance", "()L" + structureTypeFileName + ';', null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitFieldInsn(Opcodes.GETSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC, "getInstance", "()Ljcl/LispType;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, structureTypeFactoryInnerClass, "getInstance", "()L" + structureTypeFileName + ';', false);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
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
				"jcl/types/TypeBaseClass",
				new String[]{structureTypeFileName, "jcl/types/typespecifiers/AtomicTypeSpecifier"});

		cw.visitInnerClass(structureTypeFactoryInnerClass, structureTypeFileName, "Factory", Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplInnerClass, structureTypeFactoryInnerClass, structureTypeImplClassName, Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeSyntheticInnerClass, null, null, Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);

		cw.visitSource(className + ".java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitLdcInsn(structureName);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/types/TypeBaseClass", "<init>", "(Ljava/lang/String;)V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "hashCode", "()I", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitTypeInsn(Opcodes.NEW, "org/apache/commons/lang3/builder/HashCodeBuilder");
			mv.visitInsn(Opcodes.DUP);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "org/apache/commons/lang3/builder/HashCodeBuilder", "<init>", "()V", false);
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/types/TypeBaseClass", "hashCode", "()I", false);

			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "org/apache/commons/lang3/builder/HashCodeBuilder", "appendSuper", "(I)Lorg/apache/commons/lang3/builder/HashCodeBuilder;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "org/apache/commons/lang3/builder/HashCodeBuilder", "toHashCode", "()I", false);

			mv.visitInsn(Opcodes.IRETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "equals", "(Ljava/lang/Object;)Z", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int objStore = currentClass.getNextAvailableStore();

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

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_SYNTHETIC, "<init>", "(L" + structureTypeSyntheticInnerClass + ";)V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureTypeImplInnerClass, "<init>", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
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

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, structureClassFileName, null, includeStructureClassFileName, null);

		cw.visitSource(className + ".java", null);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "INSTANCE", 'L' + structureClassFileName + ';', null, null);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					"<init>",
					"(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int defaultConstructorSymbolArgStore = currentClass.getNextAvailableStore();
			final int printerSymbolArgStore = currentClass.getNextAvailableStore();
			final int directSuperClassesArgStore = currentClass.getNextAvailableStore();
			final int subClassesArgStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');
			mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureClassFileName,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int typeArgStore = currentClass.getNextAvailableStore();
			final int defaultConstructorSymbolArgStore = currentClass.getNextAvailableStore();
			final int printerSymbolArgStore = currentClass.getNextAvailableStore();
			final int directSuperClassesArgStore = currentClass.getNextAvailableStore();
			final int subClassesArgStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitVarInsn(Opcodes.ALOAD, typeArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					includeStructureClassFileName,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			final SymbolStruct<?> structureSymbol = defstructStruct.getStructureSymbol();
			final String packageName = structureSymbol.getSymbolPackage().getName();
			final String symbolName = structureSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);

			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);

			mv.visitFieldInsn(Opcodes.GETSTATIC, structureClassFileName, "INSTANCE", 'L' + structureClassFileName + ';');
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "setStructureClass", "(Ljcl/structures/StructureClassStruct;)V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "newInstance", "()Ljcl/structures/StructureObjectStruct;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, structureObjectFileName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureObjectFileName, "<init>", "()V", false);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, structureClassFileName);
			mv.visitInsn(Opcodes.DUP);

			final SymbolStruct<?> defaultConstructorSymbol = defstructStruct.getDefaultConstructorSymbol();
			if (defaultConstructorSymbol == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				final String packageName = defaultConstructorSymbol.getSymbolPackage().getName();
				final String symbolName = defaultConstructorSymbol.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);

				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			}

			final SymbolStruct<?> printerSymbol = defstructStruct.getPrinterSymbol();
			if (printerSymbol == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				final String packageName = printerSymbol.getSymbolPackage().getName();
				final String symbolName = printerSymbol.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);

				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			}

			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitInsn(Opcodes.ACONST_NULL);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureClassFileName,
					"<init>",
					"(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, structureClassFileName, "INSTANCE", 'L' + structureClassFileName + ';');

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
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

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, structureObjectFileName, null, "jcl/structures/StructureObjectStruct", null);

		final StructureClassStruct includeStructureClass = defstructStruct.getIncludeStructureClass();
		String includeStructureClassFileName = null;
		if (includeStructureClass != null) {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visitSource(className + ".java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureClassFileName, "INSTANCE", 'L' + structureClassFileName + ';');

			if (includeStructureClassFileName == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				mv.visitFieldInsn(Opcodes.GETSTATIC, includeStructureClassFileName, "INSTANCE", 'L' + includeStructureClassFileName + ';');
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, includeStructureClassFileName, "newInstance", "()Ljcl/structures/StructureObjectStruct;", false);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/structures/StructureObjectStruct", "<init>", "(Ljcl/structures/StructureClassStruct;Ljcl/structures/StructureObjectStruct;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureObjectFileName, "initSlotsMap", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initSlotsMap", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitFieldInsn(Opcodes.GETFIELD, structureObjectFileName, "slots", "Ljava/util/Map;");
			final int slotsFieldStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, slotsFieldStore);

			final int slotStore = currentClass.getNextAvailableStore();

			final List<SymbolStruct<?>> slots = defstructStruct.getSlots();
			for (final SymbolStruct<?> slot : slots) {
				final String packageName = slot.getSymbolPackage().getName();
				final String symbolName = slot.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);

				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, slotStore);

				mv.visitVarInsn(Opcodes.ALOAD, slotsFieldStore);
				mv.visitVarInsn(Opcodes.ALOAD, slotStore);
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", true);
				mv.visitInsn(Opcodes.POP);
			}

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}

		cw.visitEnd();

		classStack.pop();
	}
}
