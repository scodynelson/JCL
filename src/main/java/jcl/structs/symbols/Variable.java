package jcl.structs.symbols;

import jcl.structs.PackageStruct;
import jcl.structs.ReadtableStruct;
import jcl.structs.SymbolStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.readtables.GlobalReadtableStruct;
import jcl.types.Float;
import jcl.types.SingleFloat;

import java.util.ArrayList;
import java.util.List;

public class Variable {

	public static final ReadtableStruct Readtable = GlobalReadtableStruct.Readtable;
	public static int ReadBase = 10;
	public static boolean ReadSuppress;
	public static boolean ReadEval = true;
	public static final Float ReadDefaultFloatFormat = SingleFloat.INSTANCE;
	public static PackageStruct Package = GlobalPackageStruct.COMMON_LISP_USER;
	public static final List<SymbolStruct<?>> Features = new ArrayList<>();

	public static int getReadBase() {
		return ReadBase;
	}

	public static void setReadBase(final int readBase) {
		ReadBase = readBase;
	}

	public static boolean isReadSuppress() {
		return ReadSuppress;
	}

	public static void setReadSuppress(final boolean readSuppress) {
		ReadSuppress = readSuppress;
	}

	public static PackageStruct getPackage() {
		return Package;
	}

	public static void setPackage(final PackageStruct aPackage) {
		Package = aPackage;
	}
}
