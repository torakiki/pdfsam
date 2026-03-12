/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.gui.components.content.about;

import jakarta.inject.Inject;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.ui.ContentItem;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
public class AboutContentItem implements ContentItem {

    private final AboutPane pane;

    @Inject
    AboutContentItem(AboutPane pane) {
        this.pane = pane;
    }

    @Override
    public String id() {
        return "ABOUT";
    }

    @Override
    public String name() {
        return i18n().tr("About");
    }

    @Override
    public String description() {
        return null;
    }

    @Override
    public Pane panel() {
        return pane;
    }

    @Override
    public Node graphic() {
        var icon = new FontIcon(UniconsLine.INFO_CIRCLE);
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        return icon;
    }

}
