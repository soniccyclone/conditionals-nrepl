using System;
using System.IO;
using System.Text;

namespace nrepl
{
    public class CallbackBufferedWriter : TextWriter
    {
        private readonly Action<string> callback;
        private readonly StringBuilder buffer;
        private readonly int bufferSize;
        private readonly object bufferLock = new object();

        public CallbackBufferedWriter(Action<string> callback, int bufferSize)
        {
            this.callback = callback;
            this.bufferSize = bufferSize;
            this.buffer = new StringBuilder();
        }

        public override Encoding Encoding
        {
            get { return Encoding.UTF8; }
        }

        private static int Clamp(int value, int min, int max)
        {
            return Math.Min(Math.Max(value, min), max);
        }

        public override void Write(char c)
        {
            lock (bufferLock)
            {
                buffer.Append(c);
                if (c == '\n' || buffer.Length >= bufferSize)
                    MaybeFlush(false, true);
            }
        }

        public override void Write(char[] b, int off, int len)
        {
            lock (bufferLock)
            {
                int end = off + len;
                while (off < end)
                {
                    int writeLen = Clamp(bufferSize - buffer.Length, 1, len);
                    buffer.Append(b, off, writeLen);
                    if (buffer.Length >= bufferSize || Array.IndexOf(b, '\n', off, writeLen) >= 0)
                        MaybeFlush(false, true);
                    off += writeLen;
                    len -= writeLen;
                }
            }
        }

        public override void Write(string value)
        {
            if (value == null) return;
            Write(value.ToCharArray(), 0, value.Length);
        }

        public override void Flush()
        {
            lock (bufferLock)
            {
                MaybeFlush(true, true);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                lock (bufferLock)
                {
                    MaybeFlush(true, false);
                }
            }
            base.Dispose(disposing);
        }

        public override string ToString()
        {
            lock (bufferLock)
            {
                return buffer.ToString();
            }
        }

        private void MaybeFlush(bool force, bool keepHighSurrogate)
        {
            int size = buffer.Length;
            if (size == 0) return;

            string content = buffer.ToString();
            int length = content.Length;
            int lastNewlineIndex = content.LastIndexOf('\n');
            int lengthToFlush = (force || size >= bufferSize) ? length : lastNewlineIndex + 1;
            if (keepHighSurrogate && lengthToFlush > 0 && char.IsHighSurrogate(content[lengthToFlush - 1]))
                lengthToFlush--;
            if (lengthToFlush > 0)
            {
                string flushContent = content.Substring(0, lengthToFlush);
                callback(flushContent);
                buffer.Clear();
                if (lengthToFlush < length)
                {
                    buffer.Append(content, lengthToFlush, length - lengthToFlush);
                }
            }
        }
    }
}
