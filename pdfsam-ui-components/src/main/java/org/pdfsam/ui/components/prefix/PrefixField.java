/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.prefix;

import javafx.scene.control.ContextMenu;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import org.sejda.model.prefix.Prefix;

import static java.util.Optional.ofNullable;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Text field providing PDFsam complex prefix functionalities (context menu to automatically set prefix values)
 * 
 * @author Andrea Vacondio
 * 
 */
public class PrefixField extends TextField {

    private final Menu menu;

    public PrefixField() {
        this.setPromptText(i18n().tr("Prefix for the generated files names"));
        this.menu = new Menu(i18n().tr("Add prefix"));
        this.menu.setId("addPrefixMenu");
        this.menu.getItems().addAll(new PrefixMenuItem(Prefix.TIMESTAMP), new PrefixMenuItem(Prefix.BASENAME));
        this.setContextMenu(new ContextMenu(this.menu));
        setPrefWidth(300);
    }

    /**
     * Adds a {@link MenuItem} for the given prefixes to the context menu. By default {@link Prefix#TIMESTAMP} and {@link Prefix#BASENAME} are always there, adding them again will
     * result in a duplication.
     * 
     * @param prefixes
     */
    public void addMenuItemFor(Prefix... prefixes) {
        for (Prefix current : prefixes) {
            this.menu.getItems().add(new PrefixMenuItem(current));
        }
    }

    /**
     * Adds a {@link MenuItem} for the given prefixes to the context menu. By default {@link Prefix#TIMESTAMP} and {@link Prefix#BASENAME} are always there, adding them again will
     * 
     * @param prefixes
     */
    public void addMenuItemFor(String... prefixes) {
        for (String current : prefixes) {
            this.menu.getItems().add(new PrefixMenuItem(current));
        }
    }

    /**
     * Menu item adding a Sejda prefix to the {@link TextField}, possibly replacing current selection.
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class PrefixMenuItem extends MenuItem {

        private PrefixMenuItem(Prefix prefix) {
            this(ofNullable(prefix).map(Prefix::getFriendlyName)
                    .orElseThrow(() -> new IllegalArgumentException("Prefix cannot be null")));
        }

        private PrefixMenuItem(String prefix) {
            requireNotBlank(prefix, "Prefix cannot be blank");
            setText(prefix);
            setOnAction(e -> replaceSelection(prefix));
            this.setMnemonicParsing(false);
        }

    }
}
