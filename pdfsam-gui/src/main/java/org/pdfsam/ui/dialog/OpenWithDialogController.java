/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ago 2016
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
package org.pdfsam.ui.dialog;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;

import org.pdfsam.ui.InputPdfArgumentsLoadRequest;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.injector.Auto;

/**
 * Controller receiving notifications of input PDF files as application arguments and asking the user which module should be used to open them
 * 
 * @author Andrea Vacondio
 */
@Auto
public class OpenWithDialogController {

    private OpenWithDialog dialog;

    @Inject
    public OpenWithDialogController(OpenWithDialog dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void on(InputPdfArgumentsLoadRequest event) {
        if (nonNull(event)) {
            dialog.initFor(event).showAndWait();
        }
    }
}
