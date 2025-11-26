from dataclasses import dataclass
from typing import Any
from typing import TextIO

@dataclass
class Stream:
    """Lightweight stream descriptor used by the engine's stream registry."""
    handle: Any       # typically an Atom or similar identifier
    file_obj: TextIO
    mode: str          # 'read', 'write', or 'append'
    filename: str | None = None  # Filename if it's a file stream

    @property
    def is_standard_stream(self) -> bool:
        """Check if this is a standard stream (user_input, user_output, user_error)."""
        return self.filename is None

    def close(self) -> None:
        """Close the stream if it's a file stream."""
        if self.file_obj and not self.is_standard_stream:
            self.file_obj.close()