/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.pathnames.PathnameDevice;
import jcl.pathnames.PathnameDirectory;
import jcl.pathnames.PathnameDirectoryLevel;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.PathnameVersion;
import jcl.printer.PrinterVariables;
import jcl.printer.impl.LispPrinter;
import jcl.symbols.BooleanStruct;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.io.File;
import java.util.List;

@Component
public class PathnameStructPrinter implements LispPrinter<PathnameStruct> {

	@Override
	public String print(final PathnameStruct object) {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printEscape.booleanValue()) {
			stringBuilder.append("#P");
		}
		stringBuilder.append('"');

		final PathnameDevice device = object.getPathnameDevice();
		final String deviceString = device.getDevice();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append(deviceString);
			stringBuilder.append(':');
		}

		final PathnameDirectory directory = object.getPathnameDirectory();
		final List<PathnameDirectoryLevel> directoryLevels = directory.getDirectoryComponent().getDirectoryLevels();
		for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {
			stringBuilder.append(File.separatorChar);
			stringBuilder.append(directoryLevel.getDirectoryLevel());
		}

		final PathnameName name = object.getPathnameName();
		final String nameString = name.getName();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append(nameString);
		}

		final PathnameType type = object.getPathnameType();
		final String typeString = type.getType();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append('.');
			stringBuilder.append(typeString);
		}

		final PathnameVersion version = object.getPathnameVersion();
		final String versionString = version.getVersion().toString();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append('.');
			stringBuilder.append(versionString);
		}

		stringBuilder.append('"');

		return stringBuilder.toString();
	}
}
