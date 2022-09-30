/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/apr/2014
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
package org.pdfsam.ui.components.notification;

import javafx.scene.Node;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;

/**
 * Type of notifications
 *
 * @author Andrea Vacondio
 */
public enum NotificationType {
    INFO {
        @Override
        public Node getGraphic() {
            return FontIcon.of(UniconsLine.INFO_CIRCLE, DEFAULT_ICON_SIZE);
        }

        @Override
        public String getStyleClass() {
            return "notification-info";
        }
    },
    WARN {
        @Override
        public Node getGraphic() {
            return FontIcon.of(UniconsLine.EXCLAMATION_TRIANGLE, DEFAULT_ICON_SIZE);
        }

        @Override
        public String getStyleClass() {
            return "notification-warn";
        }
    },
    ERROR {
        @Override
        public Node getGraphic() {
            return FontIcon.of(UniconsLine.TIMES_CIRCLE, DEFAULT_ICON_SIZE);
        }

        @Override
        public String getStyleClass() {
            return "notification-error";
        }
    },
    GO_PRO {
        @Override
        public Node getGraphic() {
            return FontIcon.of(UniconsLine.HEART_SIGN, DEFAULT_ICON_SIZE);
        }

        @Override
        public String getStyleClass() {
            return "notification-pro";
        }
    },
    SHARE {
        @Override
        public Node getGraphic() {
            return FontIcon.of(UniconsLine.SHARE_ALT, DEFAULT_ICON_SIZE);
        }

        @Override
        public String getStyleClass() {
            return "";
        }
    };

    private static final int DEFAULT_ICON_SIZE = 32;

    public abstract Node getGraphic();

    public abstract String getStyleClass();
}
