/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09 ago 2016
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;

import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.InputPdfArgumentsLoadRequest;

/**
 * Component that gets cli input arguments and, if some PDF file is found among them, sends a request to handle them
 * 
 * @author Andrea Vacondio
 *
 */
class InputPdfArgumentsController implements Consumer<List<String>> {

    @Override
    public void accept(List<String> pdfs) {
        if (nonNull(pdfs) && !pdfs.isEmpty()) {
            InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();

            pdfs.stream().filter(s -> !s.startsWith("-")).filter(FileType.PDF::matches).map(Paths::get)
                    .filter(Files::isReadable).forEach(event.pdfs::add);

            if (!event.pdfs.isEmpty()) {
                eventStudio().broadcast(event);
            }
        }
    }
}
