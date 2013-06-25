/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.log;

import java.awt.Color;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import org.apache.commons.io.output.WriterOutputStream;
import org.apache.commons.lang3.StringUtils;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

/**
 * A Logback appender that logs on a {@link JTextPane}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class TextPaneAppender extends AppenderBase<ILoggingEvent> {
    private static final Map<Level, SimpleAttributeSet> ATTRIBUTES;
    static {
        Map<Level, SimpleAttributeSet> attributesCache = new HashMap<>();
        attributesCache.put(Level.ERROR, new SimpleAttributeSet());
        attributesCache.put(Level.WARN, new SimpleAttributeSet());
        ATTRIBUTES = Collections.unmodifiableMap(attributesCache);
        StyleConstants.setForeground(ATTRIBUTES.get(Level.ERROR), Color.RED);
        StyleConstants.setForeground(ATTRIBUTES.get(Level.WARN), Color.BLUE);
    }

    public static final JTextPane LOG_PANEL = new JTextLogPane();
    private PatternLayoutEncoder encoder;

    @Override
    public void start() {
        if (this.encoder == null) {
            addError("No layout set for the appender named [" + name + "].");
            return;
        }
        super.start();
    }

    @Override
    public void append(ILoggingEvent event) {
        StringWriter writer = new StringWriter();
        try {
            encoder.init(new WriterOutputStream(writer));
            encoder.doEncode(event);
        } catch (IOException e) {
            e.printStackTrace();
        }
        doAppendMessage(writer.toString(), event);
    }

    private void doAppendMessage(String message, ILoggingEvent event) {
        if (StringUtils.isNotBlank(message)) {
            StyledDocument document = LOG_PANEL.getStyledDocument();
            try {
                document.insertString(document.getLength(), message, ATTRIBUTES.get(event.getLevel()));

                if (event.hasCallerData()) {
                    for (StackTraceElement row : event.getCallerData()) {
                        document.insertString(document.getLength(), row.toString(), ATTRIBUTES.get(event.getLevel()));
                    }
                }
            } catch (BadLocationException e) {
                LOG_PANEL.setText(LOG_PANEL.getText() + "\n" + message);
            }
        }
    }

    public PatternLayoutEncoder getEncoder() {
        return encoder;
    }

    public void setEncoder(PatternLayoutEncoder encoder) {
        this.encoder = encoder;
    }

}
