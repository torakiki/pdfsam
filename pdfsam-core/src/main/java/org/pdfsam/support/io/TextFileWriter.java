/*
 * Created on 15/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
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
            LOG.info(String.format(DefaultI18nContext.getInstance().getI18n().tr("File %s saved."), file.getName()));
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().getI18n().tr("Error saving log file."), e);
        } finally {
            IOUtils.closeQuietly(fileWriter);
        }

    }

}
