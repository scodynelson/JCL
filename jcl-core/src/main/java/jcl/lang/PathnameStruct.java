package jcl.lang;

import java.io.File;
import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.PathnameStructImpl;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameDirectoryComponent;
import jcl.lang.pathname.PathnameDirectoryLevel;
import jcl.lang.pathname.PathnameDirectoryLevelType;
import jcl.lang.pathname.PathnameDirectoryType;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.pathname.PathnameVersionComponentType;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public interface PathnameStruct extends LispStruct {

	Path getPath();

	boolean exists();

	/**
	 * Getter for pathname {@link PathnameHost} value.
	 *
	 * @return pathname {@link PathnameHost} value
	 */
	PathnameHost getPathnameHost();

	default LispStruct pathnameHost() {
		final PathnameHost pathnameHost = getPathnameHost();
		if (pathnameHost == null) {
			return NILStruct.INSTANCE;
		}

		final String host = pathnameHost.getHost();
		final LispStruct returnValue;

		if (host == null) {
			final PathnameComponentType componentType = pathnameHost.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = StringStruct.toLispString(host);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link PathnameDevice} value.
	 *
	 * @return pathname {@link PathnameDevice} value
	 */
	PathnameDevice getPathnameDevice();

	default LispStruct pathnameDevice() {
		final PathnameDevice pathnameDevice = getPathnameDevice();
		if (pathnameDevice == null) {
			return NILStruct.INSTANCE;
		}

		final String device = pathnameDevice.getDevice();
		final LispStruct returnValue;

		if (device == null) {
			final PathnameComponentType componentType = pathnameDevice.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = StringStruct.toLispString(device);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link PathnameDirectory} value.
	 *
	 * @return pathname {@link PathnameDirectory} value
	 */
	PathnameDirectory getPathnameDirectory();

	default LispStruct pathnameDirectory() {
		final PathnameDirectory pathnameDirectory = getPathnameDirectory();
		if (pathnameDirectory == null) {
			return NILStruct.INSTANCE;
		}

		final PathnameDirectoryComponent directoryComponent = pathnameDirectory.getDirectoryComponent();
		final LispStruct returnValue;

		if (directoryComponent == null) {
			final PathnameComponentType componentType = pathnameDirectory.getComponentType();
			returnValue = componentType.getValue();
		} else {
			final List<LispStruct> directoryList = new ArrayList<>();

			final PathnameDirectoryType pathnameDirectoryType = directoryComponent.getPathnameDirectoryType();
			directoryList.add(pathnameDirectoryType.getValue());

			final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();
			for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {

				final PathnameDirectoryLevelType directoryLevelType = directoryLevel.getDirectoryLevelType();

				LispStruct directoryLevelValue = null;
				switch (directoryLevelType) {
					case WILD:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case BACK:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case UP:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case NULL:
						final String directoryLevelString = directoryLevel.getDirectoryLevel();
						directoryLevelValue = StringStruct.toLispString(directoryLevelString);
						break;
				}

				directoryList.add(directoryLevelValue);
			}

			returnValue = ListStruct.toLispList(directoryList);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link PathnameName} value.
	 *
	 * @return pathname {@link PathnameName} value
	 */
	PathnameName getPathnameName();

	default LispStruct pathnameName() {
		final PathnameName pathnameName = getPathnameName();
		if (pathnameName == null) {
			return NILStruct.INSTANCE;
		}

		final String name = pathnameName.getName();
		final LispStruct returnValue;

		if (name == null) {
			final PathnameComponentType componentType = pathnameName.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = StringStruct.toLispString(name);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link PathnameType} value.
	 *
	 * @return pathname {@link PathnameType} value
	 */
	PathnameType getPathnameType();

	default LispStruct pathnameType() {
		final PathnameType pathnameType = getPathnameType();
		if (pathnameType == null) {
			return NILStruct.INSTANCE;
		}

		final String type = pathnameType.getType();
		final LispStruct returnValue;

		if (type == null) {
			final PathnameComponentType componentType = pathnameType.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = StringStruct.toLispString(type);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link PathnameVersion} value.
	 *
	 * @return pathname {@link PathnameVersion} value
	 */
	PathnameVersion getPathnameVersion();

	default LispStruct pathnameVersion() {
		final PathnameVersion pathnameVersion = getPathnameVersion();
		if (pathnameVersion == null) {
			return NILStruct.INSTANCE;
		}

		final Integer version = pathnameVersion.getVersion();
		final LispStruct returnValue;

		if (version == null) {
			final PathnameVersionComponentType componentType = pathnameVersion.getComponentType();
			returnValue = componentType.getValue();
		} else {
			final String versionString = version.toString();
			returnValue = StringStruct.toLispString(versionString);
		}

		return returnValue;
	}

	/**
	 * Getter for pathname {@link URI} value.
	 *
	 * @return pathname {@link URI} value
	 */
	URI getUri();

	String getNamestring();

	@Override
	default boolean equal(final LispStruct object) {
		// TODO: clean this up!!!
		if (eq(object)) {
			return true;
		}
		if (object instanceof PathnameStruct) {
			final PathnameStruct p = (PathnameStruct) object;
//			if (Utilities.isPlatformWindows) {
//				if (!host.equalp(p.host)) {
//					return false;
//				}
//				if (!device.equalp(p.device)) {
//					return false;
//				}
//				if (!directory.equalp(p.directory)) {
//					return false;
//				}
//				if (!name.equalp(p.name)) {
//					return false;
//				}
//				if (!type.equalp(p.type)) {
//					return false;
//				}
//				// Ignore version component.
//				//if (!version.equalp(p.version))
//				//    return false;
//			} else {
			// Unix.
			if (!getPathnameHost().equals(p.getPathnameHost())) {
				return false;
			}
			if (!getPathnameDevice().equals(p.getPathnameDevice())) {
				return false;
			}
			if (!getPathnameDirectory().equals(p.getPathnameDirectory())) {
				return false;
			}
			if (!getPathnameName().equals(p.getPathnameName())) {
				return false;
			}
			if (!getPathnameType().equals(p.getPathnameType())) {
				return false;
			}
			// Ignore version component.
			//if (!getPathnameVersion.equals(p.getPathnameVersion))
			//    return false;
//			}
			return true;
		}
		return false;
	}

	static PathnameStruct toPathname(final Path path) {
		return new PathnameStructImpl(path);
	}

	static PathnameStruct toPathname(final File file) {
		return new PathnameStructImpl(file);
	}

	static PathnameStruct toPathname(final String pathname) {
		return new PathnameStructImpl(pathname);
	}

	static PathnameStruct toPathname(final URI uri) {
		return new PathnameStructImpl(uri);
	}

	static PathnameStruct toPathname(final PathnameHost host,
	                                 final PathnameDevice device,
	                                 final PathnameDirectory directory,
	                                 final PathnameName name,
	                                 final PathnameType type,
	                                 final PathnameVersion version) {
		return new PathnameStructImpl(host, device, directory, name, type, version);
	}

	static PathnameStruct toPathname(final LispStruct struct) {
		if (struct instanceof PathnameStruct) {
			return (PathnameStruct) struct;
		}
		if (struct instanceof StringStruct) {
			final StringStruct namestringStruct = (StringStruct) struct;
			final String namestring = namestringStruct.toJavaString();
			return toPathname(namestring);
		}
		if (struct instanceof FileStreamStruct) {
			final FileStreamStruct fileStream = (FileStreamStruct) struct;
			final Path path = fileStream.getPath();
			final File file = path.toFile();
			final String namestring = file.getAbsolutePath();
			return toPathname(namestring);
		}
		throw new TypeErrorException("Type cannot be converted to PATHNAME.");
	}

	static PathnameStruct translateLogicalPathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		if (pathnameDesignator instanceof LogicalPathnameStruct) {
			final LogicalPathnameStruct logicalPathname = (LogicalPathnameStruct) pathnameDesignator;
			pathname = logicalPathname.translateLogicalPathname();
		} else if (pathnameDesignator instanceof SynonymStreamStruct) {
			final SynonymStreamStruct synonymStream = (SynonymStreamStruct) pathnameDesignator;
			final SymbolStruct streamSymbol = synonymStream.getSymbol();
			pathname = translateLogicalPathname(streamSymbol.getValue());
		} else {
			pathname = toPathname(pathnameDesignator);
		}

		return pathname;
	}

	static PathnameStruct mergePathnames(final PathnameStruct pathname, final PathnameStruct defaultPathname) {
		final PathnameVersion defaultVersion = new PathnameVersion(PathnameVersionComponentType.NEWEST);
		return mergePathnames(pathname, defaultPathname, defaultVersion);
	}

	static PathnameStruct mergePathnames(final PathnameStruct pathname, final PathnameStruct defaultPathname,
	                                     final PathnameVersion defaultVersion) {

		PathnameHost mergedPathnameHost = pathname.getPathnameHost();
		PathnameDevice mergedPathnameDevice = pathname.getPathnameDevice();
		PathnameDirectory mergedPathnameDirectory = pathname.getPathnameDirectory();
		PathnameName mergedPathnameName = pathname.getPathnameName();
		PathnameType mergedPathnameType = pathname.getPathnameType();
		PathnameVersion mergedPathnameVersion = pathname.getPathnameVersion();

		if (mergedPathnameHost == null) {
			mergedPathnameHost = defaultPathname.getPathnameHost();
		}
		if (mergedPathnameDevice == null) {
			mergedPathnameDevice = defaultPathname.getPathnameDevice();
		}
		if (mergedPathnameDirectory == null) {
			mergedPathnameDirectory = defaultPathname.getPathnameDirectory();
		}
		if ((mergedPathnameName == null) || (mergedPathnameName.getComponentType() == PathnameComponentType.NIL)) {
			mergedPathnameName = defaultPathname.getPathnameName();
		}
		if ((mergedPathnameType == null) || (mergedPathnameType.getComponentType() == PathnameComponentType.NIL)) {
			mergedPathnameType = defaultPathname.getPathnameType();
		}
		if (mergedPathnameVersion == null) {
			mergedPathnameVersion = defaultPathname.getPathnameVersion();
		} else if (defaultVersion.getComponentType() != PathnameVersionComponentType.NIL) {
			mergedPathnameVersion = defaultVersion;
		}

		if (pathname instanceof LogicalPathnameStruct) {
			return LogicalPathnameStruct.toLogicalPathname(mergedPathnameHost, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		} else {
			return toPathname(mergedPathnameHost, mergedPathnameDevice, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		}
	}
}
