package jcl.compiler.real.icg.generator.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.icg.generator.SymbolCodeGenerator;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.DefstructSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DefstructCodeGenerator implements CodeGenerator<ListStruct> {

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

	@Autowired
	private SymbolCodeGenerator symbolCodeGenerator;

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		//Chop off %defstruct part. We don't need it.
		final ListStruct arguments = input.getRest();

		//Get the Java name of struct
		ListStruct classStuff = (ListStruct) arguments.getFirst();
		//now classStuff ~= (Defstruct12071907613439006 BAR FOO)

		final SymbolStruct<?> javaName = (SymbolStruct<?>) classStuff.getFirst();
		classStuff = classStuff.getRest();
		//now classStuff ~= (BAR FOO) or just (BAR) if no include struct

//		final DefstructSymbolStruct lispName = (DefstructSymbolStruct) classStuff.getFirst(); // TODO fix
		final SymbolStruct<?> lispName = (SymbolStruct<?>) classStuff.getFirst();
		//cache the javaName with the lispName
//		lispName.setJavaName(javaName.toString()); TODO: fix

		classStuff = classStuff.getRest();
		//now classStuff ~= (FOO) or NIL
		final DefstructSymbolStruct includeName = (DefstructSymbolStruct) classStuff.getFirst();

		final LispStruct printer = arguments.getRest().getFirst();

		final Object printerFunction;
		if ((printer instanceof SymbolStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = printer;
		} else if ((printer instanceof ListStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = null; //CompileFunction.FUNCTION.funcall(printer);
		} else {
			printerFunction = null;
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
		icgCreateDefstructImplFactory(javaName.toString(), fields.length, classBuilder);
		icgCreateDefstructImplClass(javaName.toString(), implImplementing, lispName, fields, printerFunction, includeName, includedSlotNumberAsInt, classBuilder);

		// initializing code in the enclosing lambda
		classBuilder.getEmitter().emitGetstatic(javaName + "Impl$Factory", "initialize", "Z");
		classBuilder.getEmitter().emitPop();
		classBuilder.getEmitter().emitGetstatic(javaName + "Impl", "initialize", "Z");
		classBuilder.getEmitter().emitPop();

		symbolCodeGenerator.generate(javaName, classBuilder);
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/StructureClass", "DEFSTRUCT_INDICATOR", "Llisp/common/type/Symbol;");
		classBuilder.getEmitter().emitLdc(javaName + "$Factory");
		classBuilder.getEmitter().emitInvokestatic("java/lang/Class", "forName", "(Ljava/lang/String;)", "Ljava/lang/Class;", false);
		classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Symbol", "setprop", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", true);

		// it balances something that's popping...
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	// DEFSTRUCT
	//
	//
	// Making FooStruct$Factory
	private static void icgCreateDefstructFactory(final String name, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC, name + "$Factory", null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		classBuilder.getEmitter().addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		classBuilder.getEmitter().addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// <init>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// newInstance
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		classBuilder.getEmitter().emitGetstatic(name, "factory", 'L' + name + "$AbstractFactory;");
		classBuilder.getEmitter().emitGetfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/StructureClassFactory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", true);
		classBuilder.getEmitter().emitAreturn();
		classBuilder.getEmitter().endMethod();
		classBuilder.getEmitter().endClass();
	}

	// Making FooStruct$AbstractFactory
	private static void icgCreateDefstructAbstractFactory(final String name, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name + "$AbstractFactory", null, "java/lang/Object", null);
		classBuilder.getEmitter().addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC);
		classBuilder.getEmitter().newField(0, "trueFactory", "Llisp/extensions/type/StructureClassFactory;", null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		classBuilder.getEmitter().emitLdc(1);
		classBuilder.getEmitter().emitPutstatic(name + "$AbstractFactory", "initialize", "Z");
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// <init>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitAconst_null();
		classBuilder.getEmitter().emitPutfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();
		classBuilder.getEmitter().endClass();
	}

	// Making FooStruct
	private void icgCreateDefstruct(final String name, final String[] implementing, final SymbolStruct<?> lispName, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE, name, null, "java/lang/Object", implementing);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "factory", 'L' + name + "$AbstractFactory;", null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "typeName", "Llisp/common/type/Symbol;", null, null);

		classBuilder.getEmitter().addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		classBuilder.getEmitter().addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		symbolCodeGenerator.generate(lispName, classBuilder);
		classBuilder.getEmitter().emitPutstatic(name, "typeName", "Llisp/common/type/Symbol;");
		classBuilder.getEmitter().emitNew(name + "$AbstractFactory");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInvokespecial(name + "$AbstractFactory", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitPutstatic(name, "factory", 'L' + name + "$AbstractFactory;");
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();
		classBuilder.getEmitter().endClass();
	}

	// Making FooStructImpl$Factory
	private static void icgCreateDefstructImplFactory(final String name, final int length, final JavaClassBuilder classBuilder) {
		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC, implFactoryName, null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		classBuilder.getEmitter().addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		classBuilder.getEmitter().emitLdc(1);
		classBuilder.getEmitter().emitPutstatic(implFactoryName, "initialize", "Z");
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// <init>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();

		// newInstance
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		classBuilder.getEmitter().emitNew(implName);
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitInvokespecial(implName, "<init>", "([Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAreturn();
		classBuilder.getEmitter().endMethod();
		classBuilder.getEmitter().endClass();
	}

	// Making FooStructImpl$Class.
	private void icgCreateDefstructImplClass(final String name, final String[] interfaces,
	                                         final SymbolStruct<?> lispName, final SymbolStruct<?>[] fields, final Object printer,
	                                         final DefstructSymbolStruct includedStruct, final int includedSlotNumber,
	                                         final JavaClassBuilder classBuilder) {

		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		final String abstractFactoryName = name + "$AbstractFactory";

		final String includedStructFactory;
		if (includedStruct != null) { // TODO: null or NIL
			includedStructFactory = includedStruct.getJavaName() + "$Factory";
		} else {
			includedStructFactory = null;
		}

		// class definition
		classBuilder.getEmitter().newClass(Opcodes.ACC_PUBLIC, implName, null, "lisp/system/StructureClassImpl", interfaces);
		// reference to the Factory and AbstractFactory classes
		classBuilder.getEmitter().addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		classBuilder.getEmitter().addInnerClass(abstractFactoryName, name, "AbstractFactory", Opcodes.ACC_STATIC);

		// add the static fields (trueFactory, initialize, slotCount, slotInfo, slotInitForms, and slotNames)
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "trueFactory", 'L' + implFactoryName + ';', null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotCount", "I", null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "lispName", "Llisp/common/type/Symbol;", null, null);
		classBuilder.getEmitter().newField(Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotNames", "[Llisp/common/type/Symbol;", null, null);
		if (printer instanceof SymbolStruct) {
			classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/common/type/Symbol;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function2
			classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function2;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function3
			classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function3;", null, null);
		} else {
			classBuilder.getEmitter().newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Ljava/lang/Object;", null, null);
		}

		// add the instance fields: field1, field2...
		if (includedStructFactory == null) {
			for (int i = 0; i < fields.length; i++) {
				classBuilder.getEmitter().newField(0, "field" + (i + 1), "Ljava/lang/Object;", null, null);
			}
		} else {
			for (int i = 0; i < fields.length; i++) {
				classBuilder.getEmitter().newField(0, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;", null, null);
			}
		}

		//<clinit>
		classBuilder.getEmitter().newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		classBuilder.getEmitter().emitLdc(1);
		classBuilder.getEmitter().emitPutstatic(implName, "initialize", "Z");
		// hold the slot count
		classBuilder.getEmitter().emitLdc(fields.length);
		classBuilder.getEmitter().emitPutstatic(implName, "slotCount", "I");

		// if a print option was passed in, make an instance and store it
		if (printer instanceof SymbolStruct<?>) {
			specialVariableCodeGenerator.generate((SymbolStruct<?>) printer, classBuilder);
			classBuilder.getEmitter().emitPutstatic(implName, "printDefstructFunction", "Llisp/common/type/Symbol;");
		} else if ((printer instanceof FunctionStruct) || (printer instanceof FunctionStruct)) { // TODO: Function2 || Function3
			classBuilder.getEmitter().emitNew(printer.getClass().getName());
			classBuilder.getEmitter().emitDup();
			classBuilder.getEmitter().emitInvokespecial(printer.getClass().getName(), "<init>", "()", "V", false);
			if (printer instanceof FunctionStruct) { // TODO: Function2
				classBuilder.getEmitter().emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function2;");
			} else if (printer instanceof FunctionStruct) { // TODO: Function3
				classBuilder.getEmitter().emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function3;");
			}
		}

		// hold on to the original lispName
		specialVariableCodeGenerator.generate(lispName, classBuilder);
		classBuilder.getEmitter().emitPutstatic(implName, "lispName", "Llisp/common/type/Symbol;");

		// make an instance of the nested Factory class
		classBuilder.getEmitter().emitNew(implFactoryName);
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInvokespecial(implFactoryName, "<init>", "()", "V", false);
		// now stow the Factory instance into the trueFactory static final slot
		classBuilder.getEmitter().emitPutstatic(implName, "trueFactory", 'L' + implFactoryName + ';');

		// static { FooStruct.factory.trueFactory = trueFactory; }
		classBuilder.getEmitter().emitGetstatic(name, "factory", 'L' + abstractFactoryName + ';');
		classBuilder.getEmitter().emitGetstatic(implName, "trueFactory", 'L' + implFactoryName + ';');
		classBuilder.getEmitter().emitPutfield(abstractFactoryName, "trueFactory", "Llisp/extensions/type/StructureClassFactory;");

		// initialize the slotNames field with the slot names (inline)
		classBuilder.getEmitter().emitLdc(fields.length);
		classBuilder.getEmitter().emitAnewarray("lisp/common/type/Symbol");
		for (int i = 0; i < fields.length; i++) {
			classBuilder.getEmitter().emitDup();
			classBuilder.getEmitter().emitLdc(i);
			specialVariableCodeGenerator.generate(fields[i], classBuilder);
			classBuilder.getEmitter().emitAastore();
		}
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitPutstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");

		// put the same array of slot names into the type symbol
		symbolCodeGenerator.generate(lispName, classBuilder);
		classBuilder.getEmitter().emitCheckcast("lisp/system/SymbolImpl");
		classBuilder.getEmitter().emitSwap();
		classBuilder.getEmitter().emitInvokevirtual("lisp/system/SymbolImpl", "setDefstructSlotNames", "([Llisp/common/type/Symbol;)", "V", false);
		classBuilder.getEmitter().emitReturn();
		classBuilder.getEmitter().endMethod();
		// struct impl class initialized

		//<init> for a non-included instance
		if (includedStructFactory == null) {
			//build FooStructImpl constructor here
			classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitAconst_null();      // not a Java parent reference
			classBuilder.getEmitter().emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			for (int i = 0; i < fields.length; i++) {
				classBuilder.getEmitter().emitAload(0);
				classBuilder.getEmitter().emitAload(1);
				classBuilder.getEmitter().emitLdc(i);
				classBuilder.getEmitter().emitAaload();
				classBuilder.getEmitter().emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
			}
			classBuilder.getEmitter().emitReturn();
			classBuilder.getEmitter().endMethod();

			// <init> for an included instance
		} else {
			//build BarStructImpl constructor here
			classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			classBuilder.getEmitter().emitAload(0);     // this
			classBuilder.getEmitter().emitAload(1);     // args array
			// make the parent impl
			classBuilder.getEmitter().emitInvokestatic(includedStructFactory, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", false);
			// parent struct, this
			classBuilder.getEmitter().emitCheckcast("lisp/system/StructureClassImpl"); // the parent is ok
			classBuilder.getEmitter().emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			// the impl has created a parent included impl and stashed it into the instance

			// Now it gets interesting...
			// The parent(s) have claimed some of the slots. We have to find out which is now ours.
			// We do this by asking the parents how many have they used. We just then take the
			// remaining slots (recursion is involved...).
			for (int i = 0; i < fields.length; i++) {
				classBuilder.getEmitter().emitAload(0);
				classBuilder.getEmitter().emitAload(1);
				classBuilder.getEmitter().emitLdc(i + includedSlotNumber);
				classBuilder.getEmitter().emitAaload();
				classBuilder.getEmitter().emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
			}
			classBuilder.getEmitter().emitReturn();
			classBuilder.getEmitter().endMethod();
		}

		///////////////////////////
		// GET-SLOT METHOD CODE ///
		///////////////////////////

		// getSlot(Symbol sym) method
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "getSlot", "(Llisp/common/type/Symbol;)", "Ljava/lang/Object;", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		classBuilder.getEmitter().emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

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
		classBuilder.getEmitter().emitLookupswitch(getDefLabel, getKeys, getHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				classBuilder.getEmitter().visitMethodLabel(getHandlerBlocks[i]);
				classBuilder.getEmitter().emitAload(0);
				classBuilder.getEmitter().emitGetfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				classBuilder.getEmitter().emitAreturn();
			}
		} else {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				classBuilder.getEmitter().visitMethodLabel(getHandlerBlocks[i]);
				classBuilder.getEmitter().emitAload(0);
				classBuilder.getEmitter().emitGetfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				classBuilder.getEmitter().emitAreturn();
			}
		}
		classBuilder.getEmitter().visitMethodLabel(getDefLabel);

		// the default choices
		// If this has an included component, it is delegated to the parent
		// If this is the top of a chain (or there were an included), it throws an exception
		final Label excpLabel = new Label();

		// Here is the delegation code
		classBuilder.getEmitter().emitAload(0);  // this
		classBuilder.getEmitter().emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitIfnull(excpLabel);
		// call the superclass
		classBuilder.getEmitter().emitAload(1);  // the symbol
		classBuilder.getEmitter().emitInvokevirtual("lisp/system/StructureClassImpl", "getSlot", "(Llisp/common/type/Symbol;", "Ljava/lang/Object;", false);
		classBuilder.getEmitter().emitAreturn();

		// Here is the exception code
		classBuilder.getEmitter().visitMethodLabel(excpLabel);
		classBuilder.getEmitter().emitPop();
		classBuilder.getEmitter().emitNew("lisp/common/exceptions/FunctionException");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitNew("java/lang/StringBuilder");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitLdc("Slot  ");
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitLdc(" not Found");
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		classBuilder.getEmitter().emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitAthrow();

		classBuilder.getEmitter().endMethod();

		///////////////////////////
		// SET-SLOT METHOD CODE ///
		///////////////////////////

		//setSlot method
		classBuilder.getEmitter().newMethod(Opcodes.ACC_PUBLIC, "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", null, null);
		classBuilder.getEmitter().emitAload(0);
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		classBuilder.getEmitter().emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

		final Label setDefLabel = new Label();
		final Label[] setHandlerBlocks = new Label[fields.length];
		final int[] setKeys = new int[fields.length + 1];
		for (int i = 0; i < setKeys.length; i++) {
			setKeys[i] = i;
		}
		for (int i = 0; i < setHandlerBlocks.length; i++) {
			setHandlerBlocks[i] = new Label();
		}
		classBuilder.getEmitter().emitLookupswitch(setDefLabel, setKeys, setHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				classBuilder.getEmitter().visitMethodLabel(setHandlerBlocks[i]);
				classBuilder.getEmitter().emitAload(0); // this
				classBuilder.getEmitter().emitAload(2); // the new value
				classBuilder.getEmitter().emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				classBuilder.getEmitter().emitReturn();
			}
		} else {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				classBuilder.getEmitter().visitMethodLabel(setHandlerBlocks[i]);
				classBuilder.getEmitter().emitAload(0); // this
				classBuilder.getEmitter().emitAload(2); // the new value
				classBuilder.getEmitter().emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				classBuilder.getEmitter().emitReturn();
			}
		}
		classBuilder.getEmitter().visitMethodLabel(setDefLabel);

		final Label exDefLabel = new Label();

		// Here is the delegation code
		classBuilder.getEmitter().emitAload(0);  // this
		classBuilder.getEmitter().emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitIfnull(exDefLabel);
		// call the superclass
		classBuilder.getEmitter().emitAload(1);  // the symbol
		classBuilder.getEmitter().emitAload(2); // the new value
		classBuilder.getEmitter().emitInvokevirtual("lisp/system/StructureClassImpl", "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitReturn();

		// The exception if it can't find the slot
		classBuilder.getEmitter().visitMethodLabel(exDefLabel);
		classBuilder.getEmitter().emitPop();
		classBuilder.getEmitter().emitNew("lisp/common/exceptions/FunctionException");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitNew("java/lang/StringBuilder");
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		classBuilder.getEmitter().emitLdc("Slot  ");
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitAload(1);
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitLdc(" not Found");
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		classBuilder.getEmitter().emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		classBuilder.getEmitter().emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitAthrow();

		// End of the method
		classBuilder.getEmitter().endMethod();

		// All done here.
		classBuilder.getEmitter().endClass();
	}
}
