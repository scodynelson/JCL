package jcl.compiler.real.icg.generator.defstruct;

import java.security.SecureRandom;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.functions.CompileFunction;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.defstruct.DefstructStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.structures.StructureClassStruct;
import jcl.symbols.DefstructSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.StructureObjectType;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

//@Component
public class DefstructCodeGenerator implements CodeGenerator<ListStruct> {

	public static void generateStructureType(final DefstructStruct defstructStruct) {

		final SymbolStruct<?> structureSymbol = defstructStruct.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String structureTypeFileName = "jcl/structures/type/" + structureName + "StructureType";

		final String structureTypeInnerClassName = structureTypeFileName + "$1";
		final String structureTypeInnerClassFactoryName = structureTypeFileName + "$Factory";

		final String structureTypeImplClassName = structureName + "StructureTypeImpl";
		final String structureTypeInnerClassTypeImplName = structureTypeInnerClassFactoryName + '$' + structureTypeImplClassName;

		final String className = structureTypeFileName.substring(structureTypeFileName.lastIndexOf('/') + 1, structureTypeFileName.length());

		final ClassWriter cw = new ClassWriter(0);

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

		cw.visitInnerClass(structureTypeInnerClassName, null, null, Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);
		cw.visitInnerClass(structureTypeInnerClassFactoryName, structureTypeFileName, "Factory", Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeInnerClassTypeImplName, structureTypeInnerClassFactoryName, structureTypeImplClassName, Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(className + ".java", null);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "INSTANCE", 'L' + structureTypeFileName + ';', null, null);
			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, structureTypeInnerClassTypeImplName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureTypeInnerClassTypeImplName, "<init>", "(L" + structureTypeInnerClassName + ";)V", false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		cw.visitEnd();
	}

	public static void generateStructureClass(final DefstructStruct defstructStruct) {

		final SymbolStruct<?> structureSymbol = defstructStruct.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String structureTypeFileName = "jcl/structures/type/" + structureName + "StructureType";
		final String structureClassFileName = "jcl/structures/struct/class/" + structureName + "StructureClass";
		final String structureObjectFileName = "jcl/structures/struct/object/" + structureName + "StructureObject";

		final String className = structureClassFileName.substring(structureClassFileName.lastIndexOf('/') + 1, structureClassFileName.length());

		final ClassWriter cw = new ClassWriter(0);

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
			fv.visitEnd();
		}
		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					"<init>",
					"(Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureTypeFileName, "INSTANCE", 'L' + structureTypeFileName + ';');
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitVarInsn(Opcodes.ALOAD, 2);
			mv.visitVarInsn(Opcodes.ALOAD, 3);
			mv.visitVarInsn(Opcodes.ALOAD, 4);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					structureClassFileName,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct<*>;Ljcl/symbols/SymbolStruct<*>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/LispStruct;>;>;)V",
					null);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitVarInsn(Opcodes.ALOAD, 2);
			mv.visitVarInsn(Opcodes.ALOAD, 3);
			mv.visitVarInsn(Opcodes.ALOAD, 4);
			mv.visitVarInsn(Opcodes.ALOAD, 5);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					includeStructureClassFileName,
					"<init>",
					"(Ljcl/LispType;Ljcl/symbols/SymbolStruct;Ljcl/symbols/SymbolStruct;Ljava/util/List;Ljava/util/List;)V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "newInstance", "()Ljcl/structures/StructureObjectStruct;", null, null);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, structureObjectFileName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureObjectFileName, "<init>", "()V", false);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
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
		}

		cw.visitEnd();
	}

	public static void generateStructureObject(final DefstructStruct defstructStruct) {

		final SymbolStruct<?> structureSymbol = defstructStruct.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String structureClassFileName = "jcl/structures/struct/class/" + structureName + "StructureClass";
		final String structureObjectFileName = "jcl/structures/struct/object/" + structureName + "StructureObject";

		final String className = structureObjectFileName.substring(structureObjectFileName.lastIndexOf('/') + 1, structureObjectFileName.length());

		final ClassWriter cw = new ClassWriter(0);

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
			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitFieldInsn(Opcodes.GETSTATIC, structureClassFileName, "INSTANCE", 'L' + structureClassFileName + ';');

			if (includeStructureClassFileName == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				mv.visitFieldInsn(Opcodes.GETSTATIC, includeStructureClassFileName, "INSTANCE", 'L' + includeStructureClassFileName + ';');
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, includeStructureClassFileName, "newInstance", "()Ljcl/structures/StructureObjectStruct;", false);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/structures/StructureObjectStruct", "<init>", "(Ljcl/structures/StructureClassStruct;Ljcl/structures/StructureObjectStruct;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structureObjectFileName, "initSlotsMap", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initSlotsMap", "()V", null, null);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitFieldInsn(Opcodes.GETFIELD, structureObjectFileName, "slots", "Ljava/util/Map;");
			final int slotsFieldStore = 1; // TODO: currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, slotsFieldStore);

			final int slotStore = 2; // TODO: currentClass.getNextAvailableStore();

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
		}

		cw.visitEnd();
	}


	/**
	 * This method processes all the info needed to create a new definition of struct.
	 * The arg to this method looks something like this for a struct named FOO that
	 * includes the struct named BAR:
	 * (%DEFSTRUCT (Defstruct12071904607348921 BAR FOO]) (A TYPE T) (B TYPE T))
	 * NOTE: Some important parts of this method:
	 * 1. javaName - the name a struct is known by in Java (e.g. Defstruct12071904607348921)
	 * 2. lispName - the name a struct in known by in Lisp (e.g. BAR)
	 * 3. includeName - the name of the struct that this struct includes (e.g. FOO - or may be null)
	 * When the parent struct was created we attached the javaName to the lispName symbol
	 * in the field defstructJavaName (with public getter and setter). Then we can get
	 * the included struct's javaName from the includeName symbol.
	 * 4. fieldList - this is the list of all the slot names and their types for this struct
	 */

	//	@Autowired
	private FormGenerator formGenerator;

	//	@Autowired
	private CompileFunction compileFunction;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		//Chop off %defstruct part. We don't need it.
		final ListStruct arguments = input.getRest();

		//Get the Java name of struct
		ListStruct classStuff = (ListStruct) arguments.getFirst();
		//now classStuff ~= (Defstruct12071907613439006 BAR FOO)

		final SymbolStruct<?> javaName = (SymbolStruct<?>) classStuff.getFirst();
		classStuff = classStuff.getRest();
		//now classStuff ~= (BAR FOO) or just (BAR) if no include struct

		final DefstructSymbolStruct lispName = (DefstructSymbolStruct) classStuff.getFirst();
		//cache the javaName with the lispName
		lispName.setJavaName(javaName.toString());

		classStuff = classStuff.getRest();
		//now classStuff ~= (FOO) or NIL
		final DefstructSymbolStruct includeName = (DefstructSymbolStruct) classStuff.getFirst();

		final LispStruct printer = arguments.getRest().getFirst();

		Object printerFunction = null;
		if ((printer instanceof SymbolStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = printer;
		} else if ((printer instanceof ListStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = compileFunction.compile(null, printer);
		}

		//Get field list.
		final NumberStruct includedSlotNumber = (NumberStruct) arguments.getRest().getRest().getFirst();
		final int includedSlotNumberAsInt = ((IntegerStruct) includedSlotNumber).getBigInteger().intValue();

		//Get field list.
		ListStruct fieldList = arguments.getRest().getRest().getRest();
		//fieldList now ~= ((A TYPE T) (B TYPE T))

		//interface used by the struct impl. Always javaName.
		final String[] implImplementing = {javaName.toString()};
		//interface used by the struct interface
		final String[] ifaceImplementing = new String[1];

		if (includeName == null) { // TODO: null OR NIL
			ifaceImplementing[0] = "lisp/common/type/StructureClass";
		} else {
			ifaceImplementing[0] = includeName.getJavaName();
		}

		//Process the fields (i.e. slots)
		final int fieldListSize = fieldList.size();
		final SymbolStruct<?>[] fields = new SymbolStruct<?>[fieldListSize];

		//values = new Object[fieldListSize];
		for (int i = 0; i < fieldListSize; i++) {
			//get first set of field info
			final SymbolStruct<?> tempName = (SymbolStruct<?>) fieldList.getFirst();
			//if there are more fields, get the rest of them
			fieldList = fieldList.getRest();
			//parse out the field name, type, and init value info
			fields[i] = tempName;
		}

		//these methods generate the byte code to create the struct stuff
		icgCreateDefstructFactory(javaName.toString(), classBuilder);
		icgCreateDefstructAbstractFactory(javaName.toString(), classBuilder);
		icgCreateDefstruct(javaName.toString(), ifaceImplementing, lispName, classBuilder);
		icgCreateDefstructImplFactory(javaName.toString(), classBuilder);
		icgCreateDefstructImplClass(javaName.toString(), implImplementing, lispName, fields, printerFunction, includeName, includedSlotNumberAsInt, classBuilder);

		// initializing code in the enclosing lambda
		mv.visitFieldInsn(Opcodes.GETSTATIC, javaName + "Impl$Factory", "initialize", "Z");
		mv.visitInsn(Opcodes.POP);
		mv.visitFieldInsn(Opcodes.GETSTATIC, javaName + "Impl", "initialize", "Z");
		mv.visitInsn(Opcodes.POP);

		formGenerator.generate(javaName, classBuilder);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "lisp/common/type/StructureClass", "DEFSTRUCT_INDICATOR", "Llisp/common/type/Symbol;");
		mv.visitLdcInsn(javaName + "$Factory");
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Class", "forName", "(Ljava/lang/String;)Ljava/lang/Class;", false);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/Symbol", "setprop", "(Ljava/lang/Object;Ljava/lang/Object;)V", true);

		// it balances something that's popping...
		mv.visitFieldInsn(Opcodes.GETSTATIC, "lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	// DEFSTRUCT
	//
	//
	// Making FooStruct$Factory
	private static void icgCreateDefstructFactory(final String name, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = new ClassDef(name, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name + "$Factory", null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		cw.visitInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		cw.visitInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		// <init>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		// newInstance
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "newInstance", "([Ljava/lang/Object;)Llisp/common/type/StructureClass;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitFieldInsn(Opcodes.GETSTATIC, name, "factory", 'L' + name + "$AbstractFactory;");
			mv.visitFieldInsn(Opcodes.GETFIELD, name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/extensions/type/StructureClassFactory", "newInstance", "([Ljava/lang/Object;)Llisp/common/type/StructureClass;", true);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
		}
	}

	// Making FooStruct$AbstractFactory
	private static void icgCreateDefstructAbstractFactory(final String name, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = new ClassDef(name, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name + "$AbstractFactory", null, "java/lang/Object", null);
		cw.visitInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC);

		{
			final FieldVisitor fv = cw.visitField(0, "trueFactory", "Llisp/extensions/type/StructureClassFactory;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}
		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		// <clinit>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitLdcInsn(1);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, name + "$AbstractFactory", "initialize", "Z");
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		// <init>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitFieldInsn(Opcodes.PUTFIELD, name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
		}
	}

	// Making FooStruct
	private void icgCreateDefstruct(final String name, final String[] implementing, final SymbolStruct<?> lispName, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = new ClassDef(name, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE, name, null, "java/lang/Object", implementing);
		cw.visitInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		cw.visitInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "factory", 'L' + name + "$AbstractFactory;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}
		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "typeName", "Llisp/common/type/Symbol;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		// <clinit>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			formGenerator.generate(lispName, classBuilder);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, name, "typeName", "Llisp/common/type/Symbol;");
			mv.visitTypeInsn(Opcodes.NEW, name + "$AbstractFactory");
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, name + "$AbstractFactory", "<init>", "()V", false);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, name, "factory", 'L' + name + "$AbstractFactory;");
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
		}
	}

	// Making FooStructImpl$Factory
	private static void icgCreateDefstructImplFactory(final String name, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = new ClassDef(name, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, implFactoryName, null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		cw.visitInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		// <clinit>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitLdcInsn(1);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implFactoryName, "initialize", "Z");
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		// <init>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		// newInstance
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "newInstance", "([Ljava/lang/Object;)Llisp/common/type/StructureClass;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitTypeInsn(Opcodes.NEW, implName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, implName, "<init>", "([Ljava/lang/Object;)V", false);
			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
		}
	}

	// Making FooStructImpl$Class.
	private void icgCreateDefstructImplClass(final String name, final String[] interfaces, final SymbolStruct<?> lispName,
	                                         final SymbolStruct<?>[] fields, final Object printer,
	                                         final DefstructSymbolStruct includedStruct, final int includedSlotNumber,
	                                         final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = new ClassDef(name, "");
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		final String abstractFactoryName = name + "$AbstractFactory";

		String includedStructFactory = null;
		if (includedStruct != null) { // TODO: null or NIL
			includedStructFactory = includedStruct.getJavaName() + "$Factory";
		}

		// class definition
		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, implName, null, "lisp/system/StructureClassImpl", interfaces);
		// reference to the Factory and AbstractFactory classes
		cw.visitInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		cw.visitInnerClass(abstractFactoryName, name, "AbstractFactory", Opcodes.ACC_STATIC);

		// add the static fields (trueFactory, initialize, slotCount, slotInfo, slotInitForms, and slotNames)
		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "trueFactory", 'L' + implFactoryName + ';', null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotCount", "I", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "lispName", "Llisp/common/type/Symbol;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		{
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotNames", "[Llisp/common/type/Symbol;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		if (printer instanceof SymbolStruct) {
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/common/type/Symbol;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		} else if (printer instanceof FunctionStruct) {
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function;", null, null);
			currentClass.setFieldVisitor(fv);
			fv.visitEnd();
		}

		// add the instance fields: field1, field2...
		if (includedStructFactory == null) {
			for (int i = 0; i < fields.length; i++) {
				final FieldVisitor fv = cw.visitField(0, "field" + (i + 1), "Ljava/lang/Object;", null, null);
				currentClass.setFieldVisitor(fv);
				fv.visitEnd();
			}
		} else {
			for (int i = 0; i < fields.length; i++) {
				final FieldVisitor fv = cw.visitField(0, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;", null, null);
				currentClass.setFieldVisitor(fv);
				fv.visitEnd();
			}
		}

		//<clinit>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitLdcInsn(1);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "initialize", "Z");
			// hold the slot count
			mv.visitLdcInsn(fields.length);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "slotCount", "I");

			// if a print option was passed in, make an instance and store it
			if (printer instanceof SymbolStruct<?>) {
				formGenerator.generate((SymbolStruct<?>) printer, classBuilder);
				mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "printDefstructFunction", "Llisp/common/type/Symbol;");
			} else if (printer instanceof FunctionStruct) {
				mv.visitTypeInsn(Opcodes.NEW, printer.getClass().getName());
				mv.visitInsn(Opcodes.DUP);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, printer.getClass().getName(), "<init>", "()V", false);
				mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "printDefstructFunction", "Llisp/extensions/type/Function;");
			}

			// hold on to the original lispName
			formGenerator.generate(lispName, classBuilder);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "lispName", "Llisp/common/type/Symbol;");

			// make an instance of the nested Factory class
			mv.visitTypeInsn(Opcodes.NEW, implFactoryName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, implFactoryName, "<init>", "()V", false);
			// now stow the Factory instance into the trueFactory static final slot
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "trueFactory", 'L' + implFactoryName + ';');

			// static { FooStruct.factory.trueFactory = trueFactory; }
			mv.visitFieldInsn(Opcodes.GETSTATIC, name, "factory", 'L' + abstractFactoryName + ';');
			mv.visitFieldInsn(Opcodes.GETSTATIC, implName, "trueFactory", 'L' + implFactoryName + ';');
			mv.visitFieldInsn(Opcodes.PUTFIELD, abstractFactoryName, "trueFactory", "Llisp/extensions/type/StructureClassFactory;");

			// initialize the slotNames field with the slot names (inline)
			mv.visitLdcInsn(fields.length);
			mv.visitTypeInsn(Opcodes.ANEWARRAY, "lisp/common/type/Symbol");
			for (int i = 0; i < fields.length; i++) {
				mv.visitInsn(Opcodes.DUP);
				mv.visitLdcInsn(i);
				formGenerator.generate(fields[i], classBuilder);
				mv.visitInsn(Opcodes.AASTORE);
			}
			mv.visitInsn(Opcodes.DUP);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, implName, "slotNames", "[Llisp/common/type/Symbol;");

			// put the same array of slot names into the type symbol
			formGenerator.generate(lispName, classBuilder);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/system/SymbolImpl");
			mv.visitInsn(Opcodes.SWAP);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/system/SymbolImpl", "setDefstructSlotNames", "([Llisp/common/type/Symbol;)V", false);
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		// struct impl class initialized

		//<init>
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			mv.visitVarInsn(Opcodes.ALOAD, 0);

			if (includedStructFactory == null) {

				//build FooStructImpl constructor here
				mv.visitInsn(Opcodes.ACONST_NULL);      // not a Java parent reference
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)V", false);
				for (int i = 0; i < fields.length; i++) {
					mv.visitVarInsn(Opcodes.ALOAD, 0);
					mv.visitVarInsn(Opcodes.ALOAD, 1);
					mv.visitLdcInsn(i);
					mv.visitInsn(Opcodes.AALOAD);
					mv.visitFieldInsn(Opcodes.PUTFIELD, implName, "field" + (i + 1), "Ljava/lang/Object;");
				}
			} else {

				//build BarStructImpl constructor here
				mv.visitVarInsn(Opcodes.ALOAD, 1);     // args array
				// make the parent impl
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, includedStructFactory, "newInstance", "([Ljava/lang/Object;)Llisp/common/type/StructureClass;", false);
				// parent struct, this
				mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/system/StructureClassImpl"); // the parent is ok
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)V", false);
				// the impl has created a parent included impl and stashed it into the instance

				// Now it gets interesting...
				// The parent(s) have claimed some of the slots. We have to find out which is now ours.
				// We do this by asking the parents how many have they used. We just then take the
				// remaining slots (recursion is involved...).
				for (int i = 0; i < fields.length; i++) {
					mv.visitVarInsn(Opcodes.ALOAD, 0);
					mv.visitVarInsn(Opcodes.ALOAD, 1);
					mv.visitLdcInsn(i + includedSlotNumber);
					mv.visitInsn(Opcodes.AALOAD);
					mv.visitFieldInsn(Opcodes.PUTFIELD, implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				}
			}
			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		///////////////////////////
		// GET-SLOT METHOD CODE ///
		///////////////////////////

		// getSlot(Symbol sym) method
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "getSlot", "(Llisp/common/type/Symbol;)Ljava/lang/Object;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitFieldInsn(Opcodes.GETSTATIC, implName, "slotNames", "[Llisp/common/type/Symbol;");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)I", false);

			// now implement the switch code that gets to the right field
			final Label getDefLabel = new Label();
			final Label[] getHandlerBlocks = new Label[fields.length];
			final int[] getKeys = new int[fields.length + 1];
			for (int i = 0; i < getKeys.length; i++) {
				getKeys[i] = i;
			}
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				getHandlerBlocks[i] = new Label();
			}
			mv.visitLookupSwitchInsn(getDefLabel, getKeys, getHandlerBlocks);
			if (includedStructFactory == null) {
				for (int i = 0; i < getHandlerBlocks.length; i++) {
					mv.visitLabel(getHandlerBlocks[i]);
					mv.visitVarInsn(Opcodes.ALOAD, 0);
					mv.visitFieldInsn(Opcodes.GETFIELD, implName, "field" + (i + 1), "Ljava/lang/Object;");
					mv.visitInsn(Opcodes.ARETURN);
				}
			} else {
				for (int i = 0; i < getHandlerBlocks.length; i++) {
					mv.visitLabel(getHandlerBlocks[i]);
					mv.visitVarInsn(Opcodes.ALOAD, 0);
					mv.visitFieldInsn(Opcodes.GETFIELD, implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
					mv.visitInsn(Opcodes.ARETURN);
				}
			}
			mv.visitLabel(getDefLabel);

			// the default choices
			// If this has an included component, it is delegated to the parent
			// If this is the top of a chain (or there were an included), it throws an exception
			final Label excpLabel = new Label();

			// Here is the delegation code
			mv.visitVarInsn(Opcodes.ALOAD, 0);  // this
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, implName, "getParent", "()Llisp/system/StructureClassImpl;", false);
			mv.visitInsn(Opcodes.DUP);
			mv.visitJumpInsn(Opcodes.IFNULL, excpLabel);
			// call the superclass
			mv.visitVarInsn(Opcodes.ALOAD, 1);  // the symbol
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/system/StructureClassImpl", "getSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;", false);
			mv.visitInsn(Opcodes.ARETURN);

			// Here is the exception code
			mv.visitLabel(excpLabel);
			mv.visitInsn(Opcodes.POP);
			mv.visitTypeInsn(Opcodes.NEW, "lisp/common/exceptions/FunctionException");
			mv.visitInsn(Opcodes.DUP);
			mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
			mv.visitLdcInsn("Slot  ");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
			mv.visitLdcInsn(" not Found");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)V", false);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		///////////////////////////
		// SET-SLOT METHOD CODE ///
		///////////////////////////

		//setSlot method
		{
			final MethodVisitor mv = currentClass.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC, "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitFieldInsn(Opcodes.GETSTATIC, implName, "slotNames", "[Llisp/common/type/Symbol;");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)I", false);

			final Label setDefLabel = new Label();
			final Label[] setHandlerBlocks = new Label[fields.length];
			final int[] setKeys = new int[fields.length + 1];
			for (int i = 0; i < setKeys.length; i++) {
				setKeys[i] = i;
			}
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				setHandlerBlocks[i] = new Label();
			}
			mv.visitLookupSwitchInsn(setDefLabel, setKeys, setHandlerBlocks);
			if (includedStructFactory == null) {
				for (int i = 0; i < setHandlerBlocks.length; i++) {
					mv.visitLabel(setHandlerBlocks[i]);
					mv.visitVarInsn(Opcodes.ALOAD, 0); // this
					mv.visitVarInsn(Opcodes.ALOAD, 2); // the new value
					mv.visitFieldInsn(Opcodes.PUTFIELD, implName, "field" + (i + 1), "Ljava/lang/Object;");
					mv.visitInsn(Opcodes.RETURN);
				}
			} else {
				for (int i = 0; i < setHandlerBlocks.length; i++) {
					mv.visitLabel(setHandlerBlocks[i]);
					mv.visitVarInsn(Opcodes.ALOAD, 0); // this
					mv.visitVarInsn(Opcodes.ALOAD, 2); // the new value
					mv.visitFieldInsn(Opcodes.PUTFIELD, implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
					mv.visitInsn(Opcodes.RETURN);
				}
			}
			mv.visitLabel(setDefLabel);

			final Label exDefLabel = new Label();

			// Here is the delegation code
			mv.visitVarInsn(Opcodes.ALOAD, 0);  // this
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, implName, "getParent", "()Llisp/system/StructureClassImpl;", false);
			mv.visitInsn(Opcodes.DUP);
			mv.visitJumpInsn(Opcodes.IFNULL, exDefLabel);
			// call the superclass
			mv.visitVarInsn(Opcodes.ALOAD, 1);  // the symbol
			mv.visitVarInsn(Opcodes.ALOAD, 2); // the new value
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/system/StructureClassImpl", "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)V", false);
			mv.visitInsn(Opcodes.RETURN);

			// The exception if it can't find the slot
			mv.visitLabel(exDefLabel);
			mv.visitInsn(Opcodes.POP);
			mv.visitTypeInsn(Opcodes.NEW, "lisp/common/exceptions/FunctionException");
			mv.visitInsn(Opcodes.DUP);
			mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
			mv.visitLdcInsn("Slot  ");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
			mv.visitLdcInsn(" not Found");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)V", false);
			mv.visitInsn(Opcodes.ATHROW);

			// End of the method
			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}

		// All done here.
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
		}
	}
}
