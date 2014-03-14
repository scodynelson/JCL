package jcl.variables;

import jcl.packages.PackageStruct;
import jcl.packages.GlobalPackageStruct;

public class PackageVariable implements LispVariable<PackageStruct> {

	public static final PackageVariable INSTANCE = new PackageVariable(GlobalPackageStruct.COMMON_LISP_USER);

	private PackageStruct value;

	private PackageVariable(final PackageStruct value) {
		this.value = value;
	}

	public PackageStruct getValue() {
		return value;
	}

	public void setValue(final PackageStruct value) {
		this.value = value;
	}
}
