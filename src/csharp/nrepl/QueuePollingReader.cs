using System;
using System.Collections.Concurrent;
using System.IO;
using System.Threading;

namespace nrepl
{
    public class QueuePollingReader : TextReader
    {
        private static readonly string EOF_MARKER = new string('\0', 1);

        private readonly Action sendNeedInputRequest;
        private readonly BlockingCollection<string> queue;

        private string currentInput = null;
        private int currentInputIndex = 0;

        public QueuePollingReader(Action sendNeedInputRequest)
        {
            this.queue = new BlockingCollection<string>();
            this.sendNeedInputRequest = sendNeedInputRequest;
        }

        private int PollInputChar()
        {
            while (true)
            {
                if (ReferenceEquals(currentInput, EOF_MARKER))
                {
                    return -2;
                }
                if (currentInput != null && currentInputIndex < currentInput.Length)
                {
                    char c = currentInput[currentInputIndex];
                    return c;
                }
                currentInputIndex = 0;
                queue.TryTake(out currentInput);
                if (currentInput == null) return -1;
            }
        }

        private int ReadInputChar()
        {
            while (true)
            {
                int c = PollInputChar();
                switch (c)
                {
                    case -2:
                        currentInput = null;
                        return -1;
                    case -1:
                        sendNeedInputRequest();
                        try { currentInput = queue.Take(); }
                        catch (ThreadInterruptedException)
                        {
                            Thread.CurrentThread.Interrupt();
                            return -1;
                        }
                        continue;
                    default:
                        currentInputIndex++;
                        return c;
                }
            }
        }

        public void AddInput(string input)
        {
            queue.Add(input);
        }

        public void AddEof()
        {
            queue.Add(EOF_MARKER);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                string discarded;
                while (queue.TryTake(out discarded)) { }
            }
            base.Dispose(disposing);
        }

        public bool Ready()
        {
            return PollInputChar() >= 0;
        }

        public override int Peek()
        {
            int c = PollInputChar();
            return c >= 0 ? c : -1;
        }

        public override int Read()
        {
            return ReadInputChar();
        }

        public override int Read(char[] buf, int off, int len)
        {
            if (len == 0) return 0;

            int firstChar = ReadInputChar();
            if (firstChar < 0) return 0;
            buf[off] = (char)firstChar;

            int i = 1;
            while (i < len)
            {
                int c = PollInputChar();
                if (c < 0) break;
                currentInputIndex++;
                buf[off + i++] = (char)c;
            }
            return i;
        }
    }
}
