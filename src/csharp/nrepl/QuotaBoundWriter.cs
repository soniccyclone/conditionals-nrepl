using System;
using System.IO;
using System.Text;

namespace nrepl
{
    public class QuotaBoundWriter : TextWriter
    {
        private readonly TextWriter wrappedWriter;
        private readonly int quota;
        private readonly object sync = new object();
        private int remaining;
        private volatile bool exceeded = false;

        public QuotaBoundWriter(TextWriter writer, int quotaChars)
        {
            if (quotaChars <= 0)
            {
                throw new ArgumentException("Invalid quota: " + quotaChars);
            }
            this.wrappedWriter = writer;
            this.quota = quotaChars;
            this.remaining = quotaChars;
        }

        public bool Exceeded
        {
            get { return exceeded; }
        }

        public override Encoding Encoding
        {
            get { return wrappedWriter.Encoding; }
        }

        public override void Write(char[] cbuf, int off, int len)
        {
            lock (sync)
            {
                bool tooBig = (len > remaining);
                len = Math.Min(len, remaining);
                if (len > 0)
                {
                    wrappedWriter.Write(cbuf, off, len);
                    remaining -= len;
                }
                if (tooBig)
                {
                    exceeded = true;
                    throw new QuotaExceededException();
                }
            }
        }

        public override void Write(string value)
        {
            if (value == null) return;
            lock (sync)
            {
                int len = value.Length;
                bool tooBig = (len > remaining);
                len = Math.Min(len, remaining);
                if (len > 0)
                {
                    wrappedWriter.Write(tooBig ? value.Substring(0, len) : value);
                    remaining -= len;
                }
                if (tooBig)
                {
                    exceeded = true;
                    throw new QuotaExceededException();
                }
            }
        }

        public override void Write(char value)
        {
            lock (sync)
            {
                int len = 1;
                bool tooBig = (len > remaining);
                len = Math.Min(len, remaining);
                if (len > 0)
                {
                    wrappedWriter.Write(value);
                    remaining -= len;
                }
                if (tooBig)
                {
                    exceeded = true;
                    throw new QuotaExceededException();
                }
            }
        }

        public override void Flush()
        {
            wrappedWriter.Flush();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                try { wrappedWriter.Flush(); }
                finally { wrappedWriter.Dispose(); }
            }
            base.Dispose(disposing);
        }

        public override string ToString()
        {
            return wrappedWriter.ToString();
        }
    }
}
