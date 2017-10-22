/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support.io;

import static org.apache.commons.lang3.StringUtils.trimToEmpty;

import java.io.BufferedWriter;
import java.io.File;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.Collection;

import javafx.scene.input.ClipboardContent;

import org.pdfsam.i18n.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component allowing to fluently write {@link Collection} of {@link Object} content to a {@link File} or {@link ClipboardContent}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class ObjectCollectionWriter implements OngoingWrite {

    public static final String SEPARATOR = System.getProperty("line.separator", "\n");

    private static final Logger LOG = LoggerFactory.getLogger(ObjectCollectionWriter.class);

    private Collection<? extends Object> content;

    private ObjectCollectionWriter(Collection<? extends Object> content) {
        this.content = content;
    }

    public static OngoingWrite writeContent(Collection<? extends Object> content) {
        return new ObjectCollectionWriter(content);
    }

    @Override
    public void to(File file) {
        try (BufferedWriter writer = Files.newBufferedWriter(file.toPath())) {
            for (Object item : content) {
                writer.append(defaultLineSeparator(item.toString()));
            }
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error saving log file."), e);
        }
        LOG.info(DefaultI18nContext.getInstance().i18n("File {0} saved.", file.getAbsolutePath()));
    }

    @Override
    public void to(ClipboardContent clipboard) {
        try (StringWriter writer = new StringWriter()) {
            for (Object item : content) {
                writer.append(defaultLineSeparator(item.toString()));
            }
            clipboard.putString(writer.toString());
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error saving log file."), e);
        }
    }

    private String defaultLineSeparator(String line) {
        return trimToEmpty(line).concat(SEPARATOR);
    }
}
