/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
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
package org.pdfsam.support.io;

import java.io.File;
import java.io.FileWriter;

import org.apache.commons.io.IOUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component allowing to fluently write {@link String} content to a {@link File}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class TextFileWriter implements OngoingFileWrite {

    private static final Logger LOG = LoggerFactory.getLogger(TextFileWriter.class);

    private String content;

    private TextFileWriter(String content) {
        this.content = content;
    }

    public static OngoingFileWrite writeContent(String content) {
        return new TextFileWriter(content);
    }

    @Override
    public void to(File file) {
        FileWriter fileWriter = null;
        try {
            fileWriter = new FileWriter(file);
            IOUtils.write(content, fileWriter);
            LOG.info(DefaultI18nContext.getInstance().i18n("File {0} saved.", file.getName()));
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error saving log file."), e);
        } finally {
            IOUtils.closeQuietly(fileWriter);
        }

    }

}
