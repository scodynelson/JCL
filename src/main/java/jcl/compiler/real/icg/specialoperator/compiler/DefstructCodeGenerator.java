package jcl.compiler.real.icg.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SymbolCodeGenerator;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.NumberStruct;
import jcl.structs.symbols.DefstructSymbolStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

public class DefstructCodeGenerator {

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
	public static void genCodeDefstruct(final IntermediateCodeGenerator icg, final ListStruct list) {
		//Chop off %defstruct part. We don't need it.
		final ListStruct arguments = list.getRest();

		//Get the Java name of struct
		ListStruct classStuff = (ListStruct) arguments.getFirst();
		//now classStuff ~= (Defstruct12071907613439006 BAR FOO)

		final SymbolStruct<?> javaName = (SymbolStruct) classStuff.getFirst();
		classStuff = classStuff.getRest();
		//now classStuff ~= (BAR FOO) or just (BAR) if no include struct

		final DefstructSymbolStruct lispName = (DefstructSymbolStruct) classStuff.getFirst();
		//cache the javaName with the lispName
		lispName.setJavaName(javaName.toString());

		classStuff = classStuff.getRest();
		//now classStuff ~= (FOO) or NIL
		final DefstructSymbolStruct includeName = (DefstructSymbolStruct) classStuff.getFirst();

		final LispStruct printer = arguments.getRest().getFirst();

		final Object printerFunction;
		if ((printer instanceof SymbolStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = printer;
		} else if ((printer instanceof ListStruct) && !printer.equals(NullStruct.INSTANCE)) {
			printerFunction = CompileFunction.FUNCTION.funcall(printer);
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
			final SymbolStruct<?> tempName = (SymbolStruct) fieldList.getFirst();
			//if there are more fields, get the rest of them
			fieldList = fieldList.getRest();
			//parse out the field name, type, and init value info
			fields[i] = tempName;
		}

		//these methods generate the byte code to create the struct stuff
		icgCreateDefstructFactory(icg, javaName.toString());
		icgCreateDefstructAbstractFactory(icg, javaName.toString());
		icgCreateDefstruct(icg, javaName.toString(), ifaceImplementing, lispName);
		icgCreateDefstructImplFactory(icg, javaName.toString(), fields.length);
		icgCreateDefstructImplClass(icg, javaName.toString(), implImplementing, lispName, fields, printerFunction, includeName, includedSlotNumberAsInt);

		// initializing code in the enclosing lambda
		icg.emitter.emitGetstatic(javaName + "Impl$Factory", "initialize", "Z");
		icg.emitter.emitPop();
		icg.emitter.emitGetstatic(javaName + "Impl", "initialize", "Z");
		icg.emitter.emitPop();

		SymbolCodeGenerator.genCodeSpecialSymbol(icg, javaName);
		icg.emitter.emitGetstatic("lisp/common/type/StructureClass", "DEFSTRUCT_INDICATOR", "Llisp/common/type/Symbol;");
		icg.emitter.emitLdc(javaName + "$Factory");
		icg.emitter.emitInvokestatic("java/lang/Class", "forName", "(Ljava/lang/String;)", "Ljava/lang/Class;", false);
		icg.emitter.emitInvokeinterface("lisp/common/type/Symbol", "setprop", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", true);

		// it balances something that's popping...
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	// DEFSTRUCT
	//
	//
	// Making FooStruct$Factory
	private static void icgCreateDefstructFactory(final IntermediateCodeGenerator icg, final String name) {
		icg.emitter.newClass(Opcodes.ACC_PUBLIC, name + "$Factory", null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		icg.emitter.addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		icg.emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		icg.emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		icg.emitter.emitReturn();
		icg.emitter.endMethod();

		// <init>
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		icg.emitter.emitAload(0);
		icg.emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		icg.emitter.emitReturn();
		icg.emitter.endMethod();

		// newInstance
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		icg.emitter.emitGetstatic(name, "factory", "L" + name + "$AbstractFactory;");
		icg.emitter.emitGetfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		icg.emitter.emitAload(0);
		icg.emitter.emitInvokeinterface("lisp/extensions/type/StructureClassFactory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", true);
		icg.emitter.emitAreturn();
		icg.emitter.endMethod();
		icg.emitter.endClass();
	}

	// Making FooStruct$AbstractFactory
	private static void icgCreateDefstructAbstractFactory(final IntermediateCodeGenerator icg, final String name) {
		icg.emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name + "$AbstractFactory", null, "java/lang/Object", null);
		icg.emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC);
		icg.emitter.newField(0, "trueFactory", "Llisp/extensions/type/StructureClassFactory;", null, null);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		icg.emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		icg.emitter.emitLdc(1);
		icg.emitter.emitPutstatic(name + "$AbstractFactory", "initialize", "Z");
		icg.emitter.emitReturn();
		icg.emitter.endMethod();

		// <init>
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		icg.emitter.emitAload(0);
		icg.emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		icg.emitter.emitAload(0);
		icg.emitter.emitAconst_null();
		icg.emitter.emitPutfield(name + "$AbstractFactory", "trueFactory", "Llisp/extensions/type/StructureClassFactory;");
		icg.emitter.emitReturn();
		icg.emitter.endMethod();
		icg.emitter.endClass();
	}

	// Making FooStruct
	private static void icgCreateDefstruct(final IntermediateCodeGenerator icg, final String name, final String[] implementing, final SymbolStruct<?> lispName) {
		icg.emitter.newClass(Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE, name, null, "java/lang/Object", implementing);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "factory", "L" + name + "$AbstractFactory;", null, null);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "typeName", "Llisp/common/type/Symbol;", null, null);

		icg.emitter.addInnerClass(name + "$Factory", name, "Factory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);
		icg.emitter.addInnerClass(name + "$AbstractFactory", name, "AbstractFactory", Opcodes.ACC_STATIC + Opcodes.ACC_PUBLIC);

		// <clinit>
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		SymbolCodeGenerator.genCodeSpecialSymbol(icg, lispName);
		icg.emitter.emitPutstatic(name, "typeName", "Llisp/common/type/Symbol;");
		icg.emitter.emitNew(name + "$AbstractFactory");
		icg.emitter.emitDup();
		icg.emitter.emitInvokespecial(name + "$AbstractFactory", "<init>", "()", "V", false);
		icg.emitter.emitPutstatic(name, "factory", "L" + name + "$AbstractFactory;");
		icg.emitter.emitReturn();
		icg.emitter.endMethod();
		icg.emitter.endClass();
	}

	// Making FooStructImpl$Factory
	public static void icgCreateDefstructImplFactory(final IntermediateCodeGenerator icg, final String name, final int length) {
		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		icg.emitter.newClass(Opcodes.ACC_PUBLIC, implFactoryName, null, "java/lang/Object", new String[]{"lisp/extensions/type/StructureClassFactory"});
		icg.emitter.addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);

		// <clinit>
		icg.emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		icg.emitter.emitLdc(1);
		icg.emitter.emitPutstatic(implFactoryName, "initialize", "Z");
		icg.emitter.emitReturn();
		icg.emitter.endMethod();

		// <init>
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "()", "V", null, null);
		icg.emitter.emitAload(0);
		icg.emitter.emitInvokespecial("java/lang/Object", "<init>", "()", "V", false);
		icg.emitter.emitReturn();
		icg.emitter.endMethod();

		// newInstance
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", null, null);
		icg.emitter.emitNew(implName);
		icg.emitter.emitDup();
		icg.emitter.emitAload(1);
		icg.emitter.emitInvokespecial(implName, "<init>", "([Ljava/lang/Object;)", "V", false);
		icg.emitter.emitAreturn();
		icg.emitter.endMethod();
		icg.emitter.endClass();
	}

	// Making FooStructImpl$Class.
	public static void icgCreateDefstructImplClass(final IntermediateCodeGenerator icg, final String name, final String[] interfaces, final SymbolStruct<?> lispName, final SymbolStruct<?>[] fields, final Object printer, final DefstructSymbolStruct includedStruct, final int includedSlotNumber) {

		final String implName = name + "Impl";
		final String implFactoryName = name + "Impl$Factory";
		final String abstractFactoryName = name + "$AbstractFactory";

		final String includedStructFactory;
		if (includedStruct != null) { // TODO: null or NIL
			includedStructFactory = includedStruct.getJavaName() + "$Factory";
		} else {
			includedStructFactory = null;
		}
		;

		// class definition
		icg.emitter.newClass(Opcodes.ACC_PUBLIC, implName, null, "lisp/system/StructureClassImpl", interfaces);
		// reference to the Factory and AbstractFactory classes
		icg.emitter.addInnerClass(implFactoryName, implName, "Factory", Opcodes.ACC_STATIC);
		icg.emitter.addInnerClass(abstractFactoryName, name, "AbstractFactory", Opcodes.ACC_STATIC);

		// add the static fields (trueFactory, initialize, slotCount, slotInfo, slotInitForms, and slotNames)
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "trueFactory", "L" + implFactoryName + ";", null, null);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "initialize", "Z", null, null);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotCount", "I", null, null);
		icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "lispName", "Llisp/common/type/Symbol;", null, null);
		icg.emitter.newField(Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "slotNames", "[Llisp/common/type/Symbol;", null, null);
		if (printer instanceof SymbolStruct) {
			icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/common/type/Symbol;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function2
			icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function2;", null, null);
		} else if (printer instanceof FunctionStruct) { // TODO: Function3
			icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Llisp/extensions/type/Function3;", null, null);
		} else {
			icg.emitter.newField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "printDefstructFunction", "Ljava/lang/Object;", null, null);
		}

		// add the instance fields: field1, field2...
		if (includedStructFactory == null) {
			for (int i = 0; i < fields.length; i++) {
				icg.emitter.newField(0, "field" + (i + 1), "Ljava/lang/Object;", null, null);
			}
		} else {
			for (int i = 0; i < fields.length; i++) {
				icg.emitter.newField(0, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;", null, null);
			}
		}

		//<clinit>
		icg.emitter.newMethod(Opcodes.ACC_STATIC, "<clinit>", "()", "V", null, null);
		icg.emitter.emitLdc(1);
		icg.emitter.emitPutstatic(implName, "initialize", "Z");
		// hold the slot count
		icg.emitter.emitLdc(fields.length);
		icg.emitter.emitPutstatic(implName, "slotCount", "I");

		// if a print option was passed in, make an instance and store it
		if (printer instanceof SymbolStruct) {
			icg.genCodeSpecialVariable((SymbolStruct<?>) printer);
			icg.emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/common/type/Symbol;");
		} else if ((printer instanceof FunctionStruct) || (printer instanceof FunctionStruct)) { // TODO: Function2 || Function3
			icg.emitter.emitNew(printer.getClass().getName());
			icg.emitter.emitDup();
			icg.emitter.emitInvokespecial(printer.getClass().getName(), "<init>", "()", "V", false);
			if (printer instanceof FunctionStruct) { // TODO: Function2
				icg.emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function2;");
			} else if (printer instanceof FunctionStruct) { // TODO: Function3
				icg.emitter.emitPutstatic(implName, "printDefstructFunction", "Llisp/extensions/type/Function3;");
			}
		}

		// hold on to the original lispName
		icg.genCodeSpecialVariable(lispName);
		icg.emitter.emitPutstatic(implName, "lispName", "Llisp/common/type/Symbol;");

		// make an instance of the nested Factory class
		icg.emitter.emitNew(implFactoryName);
		icg.emitter.emitDup();
		icg.emitter.emitInvokespecial(implFactoryName, "<init>", "()", "V", false);
		// now stow the Factory instance into the trueFactory static final slot
		icg.emitter.emitPutstatic(implName, "trueFactory", "L" + implFactoryName + ";");

		// static { FooStruct.factory.trueFactory = trueFactory; }
		icg.emitter.emitGetstatic(name, "factory", "L" + abstractFactoryName + ";");
		icg.emitter.emitGetstatic(implName, "trueFactory", "L" + implFactoryName + ";");
		icg.emitter.emitPutfield(abstractFactoryName, "trueFactory", "Llisp/extensions/type/StructureClassFactory;");

		// initialize the slotNames field with the slot names (inline)
		icg.emitter.emitLdc(fields.length);
		icg.emitter.emitAnewarray("lisp/common/type/Symbol");
		for (int i = 0; i < fields.length; i++) {
			icg.emitter.emitDup();
			icg.emitter.emitLdc(i);
			icg.genCodeSpecialVariable(fields[i]);
			icg.emitter.emitAastore();
		}
		icg.emitter.emitDup();
		icg.emitter.emitPutstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");

		// put the same array of slot names into the type symbol
		SymbolCodeGenerator.genCodeSpecialSymbol(icg, lispName);
		icg.emitter.emitCheckcast("lisp/system/SymbolImpl");
		icg.emitter.emitSwap();
		icg.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "setDefstructSlotNames", "([Llisp/common/type/Symbol;)", "V", false);
		icg.emitter.emitReturn();
		icg.emitter.endMethod();
		// struct impl class initialized

		//<init> for a non-included instance
		if (includedStructFactory == null) {
			//build FooStructImpl constructor here
			icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			icg.emitter.emitAload(0);
			icg.emitter.emitAconst_null();      // not a Java parent reference
			icg.emitter.emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			for (int i = 0; i < fields.length; i++) {
				icg.emitter.emitAload(0);
				icg.emitter.emitAload(1);
				icg.emitter.emitLdc(i);
				icg.emitter.emitAaload();
				icg.emitter.emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
			}
			icg.emitter.emitReturn();
			icg.emitter.endMethod();

			// <init> for an included instance
		} else {
			//build BarStructImpl constructor here
			icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/Object;)", "V", null, null);
			icg.emitter.emitAload(0);     // this
			icg.emitter.emitAload(1);     // args array
			// make the parent impl
			icg.emitter.emitInvokestatic(includedStructFactory, "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/StructureClass;", false);
			// parent struct, this
			icg.emitter.emitCheckcast("lisp/system/StructureClassImpl"); // the parent is ok
			icg.emitter.emitInvokespecial("lisp/system/StructureClassImpl", "<init>", "(Llisp/system/StructureClassImpl;)", "V", false);
			// the impl has created a parent included impl and stashed it into the instance

			// Now it gets interesting...
			// The parent(s) have claimed some of the slots. We have to find out which is now ours.
			// We do this by asking the parents how many have they used. We just then take the
			// remaining slots (recursion is involved...).
			for (int i = 0; i < fields.length; i++) {
				icg.emitter.emitAload(0);
				icg.emitter.emitAload(1);
				icg.emitter.emitLdc(i + includedSlotNumber);
				icg.emitter.emitAaload();
				icg.emitter.emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
			}
			icg.emitter.emitReturn();
			icg.emitter.endMethod();
		}

		///////////////////////////
		// GET-SLOT METHOD CODE ///
		///////////////////////////

		// getSlot(Symbol sym) method
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "getSlot", "(Llisp/common/type/Symbol;)", "Ljava/lang/Object;", null, null);
		icg.emitter.emitAload(0);
		icg.emitter.emitAload(1);
		icg.emitter.emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		icg.emitter.emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

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
		icg.emitter.emitLookupswitch(getDefLabel, getKeys, getHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				icg.emitter.visitMethodLabel(getHandlerBlocks[i]);
				icg.emitter.emitAload(0);
				icg.emitter.emitGetfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				icg.emitter.emitAreturn();
			}
		} else {
			for (int i = 0; i < getHandlerBlocks.length; i++) {
				icg.emitter.visitMethodLabel(getHandlerBlocks[i]);
				icg.emitter.emitAload(0);
				icg.emitter.emitGetfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				icg.emitter.emitAreturn();
			}
		}
		icg.emitter.visitMethodLabel(getDefLabel);

		// the default choices
		// If this has an included component, it is delegated to the parent
		// If this is the top of a chain (or there were an included), it throws an exception
		final Label excpLabel = new Label();

		// Here is the delegation code
		icg.emitter.emitAload(0);  // this
		icg.emitter.emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		icg.emitter.emitDup();
		icg.emitter.emitIfnull(excpLabel);
		// call the superclass
		icg.emitter.emitAload(1);  // the symbol
		icg.emitter.emitInvokevirtual("lisp/system/StructureClassImpl", "getSlot", "(Llisp/common/type/Symbol;", "Ljava/lang/Object;", false);
		icg.emitter.emitAreturn();

		// Here is the exception code
		icg.emitter.visitMethodLabel(excpLabel);
		icg.emitter.emitPop();
		icg.emitter.emitNew("lisp/common/exceptions/FunctionException");
		icg.emitter.emitDup();
		icg.emitter.emitNew("java/lang/StringBuilder");
		icg.emitter.emitDup();
		icg.emitter.emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		icg.emitter.emitLdc("Slot  ");
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitAload(1);
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitLdc(" not Found");
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		icg.emitter.emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitAthrow();

		icg.emitter.endMethod();

		///////////////////////////
		// SET-SLOT METHOD CODE ///
		///////////////////////////

		//setSlot method
		icg.emitter.newMethod(Opcodes.ACC_PUBLIC, "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", null, null);
		icg.emitter.emitAload(0);
		icg.emitter.emitAload(1);
		icg.emitter.emitGetstatic(implName, "slotNames", "[Llisp/common/type/Symbol;");
		icg.emitter.emitInvokevirtual(implName, "getSlotIndex", "(Llisp/common/type/Symbol;[Llisp/common/type/Symbol;)", "I", false);

		final Label setDefLabel = new Label();
		final Label[] setHandlerBlocks = new Label[fields.length];
		final int[] setKeys = new int[fields.length + 1];
		for (int i = 0; i < setKeys.length; i++) {
			setKeys[i] = i;
		}
		for (int i = 0; i < setHandlerBlocks.length; i++) {
			setHandlerBlocks[i] = new Label();
		}
		icg.emitter.emitLookupswitch(setDefLabel, setKeys, setHandlerBlocks);
		if (includedStructFactory == null) {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				icg.emitter.visitMethodLabel(setHandlerBlocks[i]);
				icg.emitter.emitAload(0); // this
				icg.emitter.emitAload(2); // the new value
				icg.emitter.emitPutfield(implName, "field" + (i + 1), "Ljava/lang/Object;");
				icg.emitter.emitReturn();
			}
		} else {
			for (int i = 0; i < setHandlerBlocks.length; i++) {
				icg.emitter.visitMethodLabel(setHandlerBlocks[i]);
				icg.emitter.emitAload(0); // this
				icg.emitter.emitAload(2); // the new value
				icg.emitter.emitPutfield(implName, "field" + (i + 1 + includedSlotNumber), "Ljava/lang/Object;");
				icg.emitter.emitReturn();
			}
		}
		icg.emitter.visitMethodLabel(setDefLabel);

		final Label exDefLabel = new Label();

		// Here is the delegation code
		icg.emitter.emitAload(0);  // this
		icg.emitter.emitInvokevirtual(implName, "getParent", "()", "Llisp/system/StructureClassImpl;", false);
		icg.emitter.emitDup();
		icg.emitter.emitIfnull(exDefLabel);
		// call the superclass
		icg.emitter.emitAload(1);  // the symbol
		icg.emitter.emitAload(2); // the new value
		icg.emitter.emitInvokevirtual("lisp/system/StructureClassImpl", "setSlot", "(Llisp/common/type/Symbol;Ljava/lang/Object;)", "V", false);
		icg.emitter.emitReturn();

		// The exception if it can't find the slot
		icg.emitter.visitMethodLabel(exDefLabel);
		icg.emitter.emitPop();
		icg.emitter.emitNew("lisp/common/exceptions/FunctionException");
		icg.emitter.emitDup();
		icg.emitter.emitNew("java/lang/StringBuilder");
		icg.emitter.emitDup();
		icg.emitter.emitInvokespecial("java/lang/StringBuilder", "<init>", "()", "V", false);
		icg.emitter.emitLdc("Slot  ");
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitAload(1);
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/Object;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitLdc(" not Found");
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)", "Ljava/lang/StringBuilder;", false);
		icg.emitter.emitInvokevirtual("java/lang/StringBuilder", "toString", "()", "Ljava/lang/String;", false);
		icg.emitter.emitInvokespecial("lisp/common/exceptions/FunctionException", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitAthrow();

		// End of the method
		icg.emitter.endMethod();

		// All done here.
		icg.emitter.endClass();
	}
}
