using System;
using System.IO;
using System.Text;

namespace nrepl
{
    public class TeeWriter : TextWriter
    {
        private readonly TextWriter out1;
        private readonly TextWriter out2;
        private readonly object sync = new object();
        private volatile bool flushing = false;

        public TeeWriter(TextWriter out1, TextWriter out2)
        {
            this.out1 = out1;
            this.out2 = out2;
        }

        public override Encoding Encoding
        {
            get { return out1.Encoding; }
        }

        public override void Write(char value)
        {
            lock (sync)
            {
                out1.Write(value);
                out2.Write(value);
                if (value == '\n') FlushOriginal();
            }
        }

        public override void Write(char[] b, int off, int len)
        {
            lock (sync)
            {
                out1.Write(b, off, len);
                out2.Write(b, off, len);
                if (Array.IndexOf(b, '\n', off, len) >= 0) FlushOriginal();
            }
        }

        public override void Write(string value)
        {
            if (value == null) return;
            lock (sync)
            {
                out1.Write(value);
                out2.Write(value);
                if (value.IndexOf('\n') >= 0) FlushOriginal();
            }
        }

        private void FlushOriginal()
        {
            if (!flushing)
            {
                flushing = true;
                try
                {
                    out1.Flush();
                }
                finally
                {
                    flushing = false;
                }
            }
        }

        public override void Flush()
        {
            lock (sync)
            {
                if (!flushing)
                {
                    flushing = true;
                    try
                    {
                        out1.Flush();
                        out2.Flush();
                    }
                    finally
                    {
                        flushing = false;
                    }
                }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                try { out1.Dispose(); }
                finally { out2.Dispose(); }
            }
            base.Dispose(disposing);
        }
    }
}
