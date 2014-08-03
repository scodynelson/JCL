package jcl.variables;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;

public class PackageVariable implements LispVariable<PackageStruct> {

	public static final PackageVariable INSTANCE = new PackageVariable(GlobalPackageStruct.COMMON_LISP_USER);

	private PackageStruct value;

	private PackageVariable(final PackageStruct value) {
		this.value = value;
	}

	@Override
	public PackageStruct getValue() {
		return value;
	}

	public void setValue(final PackageStruct value) {
		this.value = value;
	}
}
