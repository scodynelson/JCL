package jcl.structs.pathnames;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.pathnames.Pathname;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public abstract class PathnameStruct extends BuiltInClassStruct {

	protected final PathnameHost host;
	protected final PathnameDevice device;
	protected final PathnameDirectory directory;
	protected final PathnameName name;
	protected final PathnameType type;
	protected final PathnameVersion version;

	protected PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(Pathname.INSTANCE, null, null);
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
	}

	public PathnameHost getPathnameHost() {
		return host;
	}

	public PathnameDevice getPathnameDevice() {
		return device;
	}

	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	public PathnameName getPathnameName() {
		return name;
	}

	public PathnameType getPathnameType() {
		return type;
	}

	public PathnameVersion getPathnameVersion() {
		return version;
	}

	@Override
	public String toString() {
		return "PathnameStruct{"
				+ "\nhost=" + host
				+ ", \ndevice=" + device
				+ ", \ndirectory=" + directory
				+ ", \nname=" + name
				+ ", \ntype=" + type
				+ ", \nversion=" + version
				+ '}';
	}

	public static PathnameStruct buildPathname(final String pathname) {
		if (isURI(pathname)) {
			return new PathnameURIStruct(pathname);
		} else {
			return new PathnameFileStruct(pathname);
		}
	}

	private static boolean isURI(final String path) {
		try {
			final URI uri = new URI(path);
			return uri.isAbsolute();
		} catch (final URISyntaxException ignore) {
			return false;
		}
	}

	protected static class PathnameHost {

		private final String host;

		protected PathnameHost(final String host) {
			this.host = host;
		}

		public String getHost() {
			return host;
		}

		@Override
		public String toString() {
			return "PathnameHost{"
					+ "\nhost=" + host
					+ '}';
		}
	}

	protected static class PathnameDevice {

		private final String device;
		private final PathnameComponentType componentType;

		protected PathnameDevice(final String device) {
			this.device = device;

			if (device == null) {
				componentType = PathnameComponentType.NIL;
			} else if (device.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(device)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getDevice() {
			return device;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameDevice{"
					+ "\ndevice=" + device
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	protected static class PathnameDirectory {

		private final List<String> directory;
		private final PathnameDirectoryType pathnameDirectoryType;

		protected PathnameDirectory(final List<String> directory, final PathnameDirectoryType pathnameDirectoryType) {
			this.directory = directory;
			this.pathnameDirectoryType = pathnameDirectoryType;
		}

		public List<String> getDirectory() {
			return directory;
		}

		public PathnameDirectoryType getPathnameDirectoryType() {
			return pathnameDirectoryType;
		}

		@Override
		public String toString() {
			return "PathnameDirectory{"
					+ "\ndirectory=" + directory
					+ ", \npathnameDirectoryType=" + pathnameDirectoryType
					+ '}';
		}
	}

	protected static class PathnameName {

		private final String name;
		private final PathnameComponentType componentType;

		protected PathnameName(final String name) {
			this.name = name;

			if (name == null) {
				componentType = PathnameComponentType.NIL;
			} else if (name.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(name)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getName() {
			return name;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameName{"
					+ "\nname=" + name
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	protected static class PathnameType {

		private final String type;
		private final PathnameComponentType componentType;

		protected PathnameType(final String type) {
			this.type = type;

			if (type == null) {
				componentType = PathnameComponentType.NIL;
			} else if (type.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(type)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getType() {
			return type;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameType{"
					+ "\ntype=" + type
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	protected static class PathnameVersion {

		private final Integer version;
		private final PathnameComponentType componentType;

		protected PathnameVersion(final Integer version) {
			this.version = version;

			if (version == null) {
				componentType = PathnameComponentType.NEWEST;
			} else {
				componentType = PathnameComponentType.UNSPECIFIC;
			}
		}

		public Integer getVersion() {
			return version;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameVersion{"
					+ "\nversion=" + version
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	protected enum PathnameDeviceType {
		FILE
	}

	protected enum PathnameDirectoryType {
		ABSOLUTE,
		RELATIVE
	}

	protected enum PathnameDirectoryDirectionType {
		BACK,
		UP
	}

	protected enum PathnameComponentType {
		UNSPECIFIC,
		WILD,
		NEWEST,
		OLDEST,
		PREVIOUS,
		INSTALLED,
		NIL // TODO: or should we just pass null???
	}

	protected enum PathnameCase {
		COMMON,
		LOCAL
	}
}
