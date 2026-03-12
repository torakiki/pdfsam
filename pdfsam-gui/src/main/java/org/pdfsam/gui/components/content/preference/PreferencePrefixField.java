/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/10/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.preference;

import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.ui.components.prefix.PrefixField;
import org.sejda.model.prefix.Prefix;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
public class PreferencePrefixField extends PrefixField {

    PreferencePrefixField() {
        this(app());
    }

    PreferencePrefixField(ApplicationContext context) {
        setText(context.persistentSettings().get(StringPersistentProperty.PREFIX).orElse("PDFsam_"));
        setAccessibleText(i18n().tr("Default prefix for output file names"));
        setAccessibleHelp(i18n().tr("Right click to add special prefix keywords"));
        addMenuItemFor(Prefix.FILENUMBER);
        addMenuItemFor(Prefix.CURRENTPAGE);
        addMenuItemFor(Prefix.BOOKMARK);
        addMenuItemFor(Prefix.BOOKMARK_STRICT);
        addMenuItemFor("[TOTAL_FILESNUMBER]");
        focusedProperty().addListener((o, oldVal, newVal) -> {
            if (!newVal) {
                context.persistentSettings().set(StringPersistentProperty.PREFIX, getText());
            }
        });
    }
}
