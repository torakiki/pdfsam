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
package org.pdfsam.core.support.io;

import javafx.scene.input.ClipboardContent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

import static org.apache.commons.lang3.StringUtils.trimToEmpty;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component allowing to fluently write {@link Collection} of {@link Object} content to a {@link File} or {@link ClipboardContent}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class ObjectCollectionWriter implements OngoingWrite {

    public static final String SEPARATOR = System.getProperty("line.separator", "\n");

    private static final Logger LOG = LoggerFactory.getLogger(ObjectCollectionWriter.class);

    private final Collection<?> content;

    private ObjectCollectionWriter(Collection<?> content) {
        this.content = content;
    }

    public static OngoingWrite writeContent(Collection<?> content) {
        return new ObjectCollectionWriter(content);
    }

    @Override
    public void to(Path file) {
        try (BufferedWriter writer = Files.newBufferedWriter(file)) {
            for (Object item : content) {
                writer.append(defaultLineSeparator(item.toString()));
            }
        } catch (Exception e) {
            LOG.error(i18n().tr("Error saving log file."), e);
        }
        LOG.info(i18n().tr("File {0} saved.", file.toString()));
    }

    @Override
    public void to(ClipboardContent clipboard) {
        try (StringWriter writer = new StringWriter()) {
            for (Object item : content) {
                writer.append(defaultLineSeparator(item.toString()));
            }
            clipboard.putString(writer.toString());
        } catch (Exception e) {
            LOG.error(i18n().tr("Error saving log file."), e);
        }
    }

    private String defaultLineSeparator(String line) {
        return trimToEmpty(line).concat(SEPARATOR);
    }
}
