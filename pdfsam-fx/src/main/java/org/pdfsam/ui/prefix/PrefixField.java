/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.prefix;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.sejda.model.prefix.Prefix;

/**
 * Text field providing PDFsam complex prefix functionalities (context menu to automatically set prefix values)
 * 
 * @author Andrea Vacondio
 * 
 */
public class PrefixField extends TextField {

    private Menu menu;

    public PrefixField() {
        super("PDFsam_");
        this.setPromptText(DefaultI18nContext.getInstance().i18n("Prefix for the generated files names"));
        this.menu = new Menu(DefaultI18nContext.getInstance().i18n("Add prefix"));
        this.menu.getItems().addAll(new PrefixMenuItem(Prefix.TIMESTAMP), new PrefixMenuItem(Prefix.BASENAME));
        this.setContextMenu(new ContextMenu(this.menu));
        installTooltip();
    }

    private void installTooltip() {
        I18nContext ctx = DefaultI18nContext.getInstance();
        StringBuilder sb = new StringBuilder(ctx.i18n("Prefix for the output files name."));
        sb.append("\n");
        sb.append(ctx.i18n("Some special keywords are replaced with runtime values."));
        sb.append("\n");
        sb.append(ctx.i18n("Right click to add these keywords."));
        this.setTooltip(new Tooltip(sb.toString()));
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
     * Menu item adding a Sejda prefix to the {@link TextField}, possibly replacing current selection.
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class PrefixMenuItem extends MenuItem {

        private Prefix prefix;

        private PrefixMenuItem(Prefix prefix) {
            requireNotNull(prefix, "Prefix cannot be null");
            this.prefix = prefix;
            setText(prefix.getFriendlyName());
            setOnAction(new EventHandler<ActionEvent>() {

                @Override
                public void handle(ActionEvent action) {
                    replaceSelection(PrefixMenuItem.this.prefix.getFriendlyName());
                }
            });
        }

    }
}
